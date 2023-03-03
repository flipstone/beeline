{-# LANGUAGE OverloadedStrings #-}

module Beeline.HTTP.Client.RequestDefinition
  ( RequestDefinition
      ( RequestDefinition
      , requestRoute
      , requestSchema
      , responseSchemas
      )
  , RequestSchema
    ( RequestSchema
    , requestSchemaHeaders
    , encodeRequestBody
    )
  , encodeRequestWith
  , noRequestBody
  , ResponseSchema
    ( ResponseSchema
    , responseSchemaRequestHeaders
    , parseHTTPResponse
    )
  , decodeResponseWith
  , StatusRange
    ( Status
    , Informational
    , Success
    , Redirect
    , ClientError
    , ServerError
    )
  , checkStatus
  , noResponseBody
  , defaultRequestDefinition
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
import qualified Beeline.Routing as R

data RequestDefinition err route request response = RequestDefinition
  { requestRoute :: R.RouteGenerator route
  , requestSchema :: RequestSchema request
  , responseSchemas :: [(StatusRange, ResponseSchema err response)]
  }

data RequestSchema a = RequestSchema
  { requestSchemaHeaders :: [HTTPTypes.Header]
  , encodeRequestBody :: a -> HTTP.RequestBody
  }

noRequestBody :: RequestSchema ()
noRequestBody =
  RequestSchema
    { requestSchemaHeaders = []
    , encodeRequestBody = (\() -> "")
    }

encodeRequestWith :: ContentTypeEncoder coder => coder -> EncodeSchema coder a -> RequestSchema a
encodeRequestWith coder encoder =
  RequestSchema
    { requestSchemaHeaders = [("Content-Type", toRequestContentType coder encoder)]
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

data ResponseSchema err a = ResponseSchema
  { responseSchemaRequestHeaders :: [HTTPTypes.Header]
  , parseHTTPResponse :: HTTP.Response HTTP.BodyReader -> IO (Either err a)
  }

instance Functor (ResponseSchema err) where
  fmap f schema =
    schema
      { parseHTTPResponse = fmap (fmap f) . parseHTTPResponse schema
      }

noResponseBody :: ResponseSchema err ()
noResponseBody =
  ResponseSchema
    { responseSchemaRequestHeaders = []
    , parseHTTPResponse = \_response -> pure (Right ())
    }

decodeResponseWith ::
  ContentTypeDecoder coder =>
  coder ->
  DecodeSchema coder a ->
  ResponseSchema (DecodingError coder) a
decodeResponseWith coder decoder =
  ResponseSchema
    { responseSchemaRequestHeaders = [("Accept", toResponseContentType coder decoder)]
    , parseHTTPResponse = parseResponse coder decoder . HTTP.responseBody
    }

defaultRequestDefinition :: RequestDefinition err () () ()
defaultRequestDefinition =
  RequestDefinition
    { requestRoute = R.route () (R.end HTTPTypes.GET)
    , requestSchema = noRequestBody
    , responseSchemas = [(Success, noResponseBody)]
    }
