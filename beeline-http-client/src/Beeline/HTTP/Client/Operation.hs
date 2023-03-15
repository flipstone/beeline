{-# LANGUAGE OverloadedStrings #-}

module Beeline.HTTP.Client.Operation
  ( Operation
      ( Operation
      , requestRoute
      , requestQuerySchema
      , requestBodySchema
      , responseSchemas
      )
  , RequestBodySchema
    ( RequestBodySchema
    , requestBodyContentType
    , encodeRequestBody
    )
  , NoRequestBody (NoRequestBody)
  , requestBody
  , noRequestBody
  , ResponseBodySchema
    ( ResponseBodySchema
    , responseAcceptableContentTypes
    , parseHTTPResponse
    )
  , NoResponseBody (NoResponseBody)
  , noResponseBody
  , responseBody
  , NoPathParams (NoPathParams)
  , noPathParams
  , NoQueryParams (NoQueryParams)
  , noQueryParams
  , StatusRange
    ( AnyStatus
    , Status
    , Informational
    , Success
    , Redirect
    , ClientError
    , ServerError
    )
  , checkStatus
  , defaultOperation
  ) where

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTPTypes

import Beeline.HTTP.Client.ContentType
  ( ContentTypeDecoder
  , ContentTypeEncoder
  , DecodeSchema
  , DecodingError
  , EncodeSchema
  , parseResponse
  , toRequestBody
  , toRequestContentType
  , toResponseContentType
  )
import Beeline.HTTP.Client.QuerySchema
  ( QueryEncoder
  , QuerySchema (makeQuery)
  )

import qualified Beeline.Routing as R

data Operation err route query requestBody response = Operation
  { requestRoute :: R.RouteGenerator route
  , requestQuerySchema :: QueryEncoder query query
  , requestBodySchema :: RequestBodySchema requestBody
  , responseSchemas :: [(StatusRange, ResponseBodySchema err response)]
  }

data RequestBodySchema a = RequestBodySchema
  { requestBodyContentType :: Maybe BS.ByteString
  , encodeRequestBody :: a -> HTTP.RequestBody
  }

data NoRequestBody = NoRequestBody
  deriving (Show, Eq)

noRequestBody :: RequestBodySchema NoRequestBody
noRequestBody =
  RequestBodySchema
    { requestBodyContentType = Nothing
    , encodeRequestBody = (\NoRequestBody -> "")
    }

requestBody ::
  ContentTypeEncoder coder => coder -> EncodeSchema coder a -> RequestBodySchema a
requestBody coder encoder =
  RequestBodySchema
    { requestBodyContentType = Just (toRequestContentType coder encoder)
    , encodeRequestBody = toRequestBody coder encoder
    }

data StatusRange
  = AnyStatus
  | Status Int
  | Informational
  | Success
  | Redirect
  | ClientError
  | ServerError

checkStatus :: StatusRange -> HTTPTypes.Status -> Bool
checkStatus range status =
  case range of
    AnyStatus -> True
    Status code -> HTTPTypes.statusCode status == code
    Informational -> HTTPTypes.statusIsInformational status
    Success -> HTTPTypes.statusIsSuccessful status
    Redirect -> HTTPTypes.statusIsRedirection status
    ClientError -> HTTPTypes.statusIsClientError status
    ServerError -> HTTPTypes.statusIsServerError status

data ResponseBodySchema err a = ResponseBodySchema
  { responseAcceptableContentTypes :: Set.Set BS.ByteString
  , parseHTTPResponse :: HTTP.Response HTTP.BodyReader -> IO (Either err a)
  }

instance Functor (ResponseBodySchema err) where
  fmap f schema =
    schema
      { parseHTTPResponse = fmap (fmap f) . parseHTTPResponse schema
      }

data NoResponseBody = NoResponseBody
  deriving (Show, Eq)

noResponseBody :: ResponseBodySchema err NoResponseBody
noResponseBody =
  ResponseBodySchema
    { responseAcceptableContentTypes = Set.empty
    , parseHTTPResponse = \_response -> pure (Right NoResponseBody)
    }

responseBody ::
  ContentTypeDecoder coder =>
  coder ->
  DecodeSchema coder a ->
  ResponseBodySchema (DecodingError coder) a
responseBody coder decoder =
  ResponseBodySchema
    { responseAcceptableContentTypes = Set.singleton (toResponseContentType coder decoder)
    , parseHTTPResponse = parseResponse coder decoder . HTTP.responseBody
    }

data NoPathParams = NoPathParams
  deriving (Show, Eq)

noPathParams :: R.Router r => r NoPathParams
noPathParams =
  R.get (R.make NoPathParams)

data NoQueryParams = NoQueryParams
  deriving (Show, Eq)

noQueryParams :: QuerySchema q => q NoQueryParams NoQueryParams
noQueryParams =
  makeQuery NoQueryParams

defaultOperation :: Operation err NoPathParams NoQueryParams NoRequestBody NoResponseBody
defaultOperation =
  Operation
    { requestRoute = noPathParams
    , requestQuerySchema = noQueryParams
    , requestBodySchema = noRequestBody
    , responseSchemas = [(Success, noResponseBody)]
    }
