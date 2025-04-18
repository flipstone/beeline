{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright : Flipstone Technology Partners 2023-2025
License   : MIT

@since 0.1.3.0
-}
module Beeline.HTTP.Client.HTTPRequest
  ( httpRequest
  , httpRequestThrow
  , httpRequestUsing
  , throwStatusAndDecodingErrors
  , StatusResult (ExpectedStatus, UnexpectedStatus)
  , Request (Request, baseURI, baseHTTPRequest, route, query, headers, additionalHeaders, body)
  , defaultRequest
  , buildHTTPRequest
  , handleHTTPResponse
  ) where

import qualified Control.Exception as Exc
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Enc
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTPTypes

import Beeline.HTTP.Client.BaseURI (BaseURI, basePath, defaultBaseURI, host, port, secure)
import Beeline.HTTP.Client.Operation
  ( NoHeaderParams (NoHeaderParams)
  , NoPathParams (NoPathParams)
  , NoQueryParams (NoQueryParams)
  , NoRequestBody (NoRequestBody)
  , Operation
  , ResponseBodySchema
  , StatusRange
  , checkStatus
  , encodeRequestBody
  , parseHTTPResponse
  , requestBodyContentType
  , requestBodySchema
  , requestHeaderSchema
  , requestQuerySchema
  , requestRoute
  , responseAcceptableContentTypes
  , responseSchemas
  )
import Beeline.HTTP.Client.ParameterCollectionSchema (encodeHeaders, encodeQuery)
import qualified Beeline.Routing as R

data StatusResult unexpectedStatusBody err response
  = ExpectedStatus HTTP.Request (HTTP.Response ()) (Either err response)
  | UnexpectedStatus HTTP.Request (HTTP.Response unexpectedStatusBody)

data Request route query headers body = Request
  { baseURI :: BaseURI
  , baseHTTPRequest :: HTTP.Request
  , headers :: headers
  , additionalHeaders :: [HTTPTypes.Header]
  , route :: route
  , query :: query
  , body :: body
  }

defaultRequest :: Request NoPathParams NoQueryParams NoHeaderParams NoRequestBody
defaultRequest =
  Request
    { baseURI = defaultBaseURI
    , baseHTTPRequest = HTTP.defaultRequest
    , route = NoPathParams
    , query = NoQueryParams
    , headers = NoHeaderParams
    , additionalHeaders = []
    , body = NoRequestBody
    }

httpRequestThrow ::
  Exc.Exception err =>
  Operation err route query headers requestBody response ->
  Request route query headers requestBody ->
  HTTP.Manager ->
  IO response
httpRequestThrow =
  httpRequestUsing HTTP.withResponse throwStatusAndDecodingErrors

throwStatusAndDecodingErrors ::
  Exc.Exception err =>
  StatusResult HTTP.BodyReader err response ->
  IO response
throwStatusAndDecodingErrors statusResult =
  case statusResult of
    ExpectedStatus _request _response (Right result) ->
      pure result
    ExpectedStatus _request _response (Left err) ->
      Exc.throwIO err
    UnexpectedStatus request response -> do
      chunk <- HTTP.brReadSome (HTTP.responseBody response) 1024

      Exc.throwIO
        . HTTP.HttpExceptionRequest request
        . HTTP.StatusCodeException (removeBody response)
        . LBS.toStrict
        $ chunk

httpRequest ::
  Operation err route query headers requestBody response ->
  Request route query headers requestBody ->
  HTTP.Manager ->
  IO (StatusResult () err response)
httpRequest =
  let
    removeUnexpectedBody :: Applicative f => StatusResult a err response -> f (StatusResult () err response)
    removeUnexpectedBody statusResult =
      pure $
        case statusResult of
          ExpectedStatus request response decodingResult ->
            ExpectedStatus request response decodingResult
          UnexpectedStatus request response ->
            UnexpectedStatus request (removeBody response)
  in
    httpRequestUsing
      HTTP.withResponse
      removeUnexpectedBody

httpRequestUsing ::
  (HTTP.Request -> HTTP.Manager -> (HTTP.Response HTTP.BodyReader -> IO result) -> IO result) ->
  (StatusResult HTTP.BodyReader err response -> IO result) ->
  Operation err route query headers requestBody response ->
  Request route query headers requestBody ->
  HTTP.Manager ->
  IO result
httpRequestUsing runRequest handleResult operation request manager =
  let
    completeRequest =
      buildHTTPRequest operation request

    rspSchemas =
      responseSchemas operation
  in
    runRequest completeRequest manager $ \response -> do
      result <- handleHTTPResponse completeRequest rspSchemas response
      handleResult result

buildHTTPRequest ::
  Operation err route query headers body responses ->
  Request route query headers body ->
  HTTP.Request
buildHTTPRequest operation request =
  let
    querySchema =
      requestQuerySchema operation

    headerSchema =
      requestHeaderSchema operation

    rqBodySchema =
      requestBodySchema operation

    rspSchemas =
      responseSchemas operation

    requestBody =
      encodeRequestBody rqBodySchema (body request)

    acceptableContentTypes =
      Set.toList
        . foldMap (responseAcceptableContentTypes . snd)
        $ rspSchemas

    acceptHeaders =
      case acceptableContentTypes of
        [] -> []
        _ -> [(HTTPTypes.hAccept, BS.intercalate ", " acceptableContentTypes)]

    incompleteRequest =
      baseHTTPRequest request

    requestHeaders =
      concat
        [ HTTP.requestHeaders incompleteRequest
        , encodeHeaders headerSchema (headers request)
        , additionalHeaders request
        , foldMap (\ct -> [(HTTPTypes.hContentType, ct)]) (requestBodyContentType rqBodySchema)
        , acceptHeaders
        ]

    (method, routePath) =
      R.generateRoute (requestRoute operation) (route request)

    fullPath =
      basePath (baseURI request) <> Enc.encodeUtf8 routePath
  in
    incompleteRequest
      { HTTP.method = HTTPTypes.renderStdMethod method
      , HTTP.secure = secure (baseURI request)
      , HTTP.host = host (baseURI request)
      , HTTP.port = port (baseURI request)
      , HTTP.path = fullPath
      , HTTP.queryString = encodeQuery querySchema (query request)
      , HTTP.requestBody = requestBody
      , HTTP.requestHeaders = requestHeaders
      }

handleHTTPResponse ::
  Foldable t =>
  HTTP.Request ->
  t (StatusRange, ResponseBodySchema err response) ->
  HTTP.Response HTTP.BodyReader ->
  IO (StatusResult HTTP.BodyReader err response)
handleHTTPResponse completeRequest rspSchemas response = do
  let
    mbResponseSchema =
      List.find
        (\(range, _schema) -> checkStatus range (HTTP.responseStatus response))
        rspSchemas

  case mbResponseSchema of
    Nothing -> pure (UnexpectedStatus completeRequest response)
    Just (_range, schema) -> do
      decodingResult <- parseHTTPResponse schema response
      pure $
        ExpectedStatus
          completeRequest
          (removeBody response)
          decodingResult

removeBody :: HTTP.Response a -> HTTP.Response ()
removeBody response =
  response
    { HTTP.responseBody = ()
    }
