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
    , requestBodySchemaHeaders
    , encodeRequestBody
    )
  , NoRequestBody (NoRequestBody)
  , requestBody
  , noRequestBody
  , ResponseBodySchema
    ( ResponseBodySchema
    , responseSchemaRequestHeaders
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
    ( Status
    , Informational
    , Success
    , Redirect
    , ClientError
    , ServerError
    )
  , checkStatus
  , defaultOperation
  ) where

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
  { requestBodySchemaHeaders :: [HTTPTypes.Header]
  , encodeRequestBody :: a -> HTTP.RequestBody
  }

data NoRequestBody = NoRequestBody
  deriving (Show, Eq)

noRequestBody :: RequestBodySchema NoRequestBody
noRequestBody =
  RequestBodySchema
    { requestBodySchemaHeaders = []
    , encodeRequestBody = (\NoRequestBody -> "")
    }

requestBody ::
  ContentTypeEncoder coder => coder -> EncodeSchema coder a -> RequestBodySchema a
requestBody coder encoder =
  RequestBodySchema
    { requestBodySchemaHeaders = [("Content-Type", toRequestContentType coder encoder)]
    , encodeRequestBody = toRequestBody coder encoder
    }

data StatusRange
  = Status Int
  | Informational
  | Success
  | Redirect
  | ClientError
  | ServerError

checkStatus :: StatusRange -> HTTPTypes.Status -> Bool
checkStatus range status =
  case range of
    Status code -> HTTPTypes.statusCode status == code
    Informational -> HTTPTypes.statusIsInformational status
    Success -> HTTPTypes.statusIsSuccessful status
    Redirect -> HTTPTypes.statusIsRedirection status
    ClientError -> HTTPTypes.statusIsClientError status
    ServerError -> HTTPTypes.statusIsServerError status

data ResponseBodySchema err a = ResponseBodySchema
  { responseSchemaRequestHeaders :: [HTTPTypes.Header]
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
    { responseSchemaRequestHeaders = []
    , parseHTTPResponse = \_response -> pure (Right NoResponseBody)
    }

responseBody ::
  ContentTypeDecoder coder =>
  coder ->
  DecodeSchema coder a ->
  ResponseBodySchema (DecodingError coder) a
responseBody coder decoder =
  ResponseBodySchema
    { responseSchemaRequestHeaders = [("Accept", toResponseContentType coder decoder)]
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
