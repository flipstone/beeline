module Beeline.HTTP.Client.HTTPRequest
  ( httpRequest
  , httpRequestThrow
  , httpRequestHandleResult
  , StatusResult (ExpectedStatus, UnexpectedStatus)
  , BaseURI (BaseURI, host, port, basePath)
  , defaultBaseURI
  , Request (Request, baseURI, headers, baseHTTPRequest, route, query, body)
  , defaultRequest
  ) where

import qualified Control.Exception as Exc
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Text.Encoding as Enc
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTPTypes

import Beeline.HTTP.Client.Operation
  ( NoPathParams (NoPathParams)
  , NoQueryParams (NoQueryParams)
  , NoRequestBody (NoRequestBody)
  , Operation
  , StatusRange (AnyStatus, ClientError, Informational, Redirect, ServerError, Status, Success)
  , checkStatus
  , encodeRequestBody
  , parseHTTPResponse
  , requestBodySchema
  , requestBodySchemaHeaders
  , requestQuerySchema
  , requestRoute
  , responseSchemaRequestHeaders
  , responseSchemas
  )
import Beeline.HTTP.Client.QuerySchema (encodeQuery)
import qualified Beeline.Routing as R

data StatusResult unexpectedStatusBody err response
  = ExpectedStatus HTTP.Request (HTTP.Response ()) (Either err response)
  | UnexpectedStatus HTTP.Request (HTTP.Response unexpectedStatusBody)

data BaseURI = BaseURI
  { host :: BS.ByteString
  , port :: Int
  , basePath :: BS.ByteString
  }

defaultBaseURI :: BaseURI
defaultBaseURI =
  BaseURI
    { host = BS8.pack "localhost"
    , port = 80
    , basePath = BS8.pack ""
    }

data Request route query body = Request
  { baseURI :: BaseURI
  , baseHTTPRequest :: HTTP.Request
  , headers :: [HTTPTypes.Header]
  , route :: route
  , query :: query
  , body :: body
  }

defaultRequest :: Request NoPathParams NoQueryParams NoRequestBody
defaultRequest =
  Request
    { baseURI = defaultBaseURI
    , headers = []
    , baseHTTPRequest = HTTP.defaultRequest
    , route = NoPathParams
    , query = NoQueryParams
    , body = NoRequestBody
    }

httpRequestThrow ::
  Exc.Exception err =>
  Operation err route query requestBody response ->
  Request route query requestBody ->
  HTTP.Manager ->
  IO response
httpRequestThrow =
  let
    throwErrors statusResult =
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
  in
    httpRequestHandleResult throwErrors

httpRequest ::
  Operation err route query requestBody response ->
  Request route query requestBody ->
  HTTP.Manager ->
  IO (StatusResult () err response)
httpRequest =
  let
    removeUnexpectedBody statusResult =
      pure $
        case statusResult of
          ExpectedStatus request response decodingResult ->
            ExpectedStatus request response decodingResult
          UnexpectedStatus request response ->
            UnexpectedStatus request (removeBody response)
  in
    httpRequestHandleResult removeUnexpectedBody

httpRequestHandleResult ::
  (StatusResult HTTP.BodyReader err response -> IO result) ->
  Operation err route query requestBody response ->
  Request route query requestBody ->
  HTTP.Manager ->
  IO result
httpRequestHandleResult handleResult definition request manager =
  let
    querySchema =
      requestQuerySchema definition

    rqBodySchema =
      requestBodySchema definition

    rspSchemas =
      responseSchemas definition

    requestBody =
      encodeRequestBody rqBodySchema (body request)

    responseSchemaHeaders (range, schema) =
      if includeHeadersInRequest range
        then responseSchemaRequestHeaders schema
        else []

    incompleteRequest =
      baseHTTPRequest request

    requestHeaders =
      concat
        [ HTTP.requestHeaders incompleteRequest
        , headers request
        , requestBodySchemaHeaders rqBodySchema
        , foldMap responseSchemaHeaders rspSchemas
        ]

    (method, routePath) =
      R.generateRoute (requestRoute definition) (route request)

    fullPath =
      basePath (baseURI request) <> Enc.encodeUtf8 routePath

    completeRequest =
      incompleteRequest
        { HTTP.method = HTTPTypes.renderStdMethod method
        , HTTP.host = host (baseURI request)
        , HTTP.port = port (baseURI request)
        , HTTP.path = fullPath
        , HTTP.queryString = encodeQuery querySchema (query request)
        , HTTP.requestBody = requestBody
        , HTTP.requestHeaders = requestHeaders
        }
  in
    HTTP.withResponse completeRequest manager $ \response -> do
      let
        mbResponseSchema =
          List.find
            (\(range, _schema) -> checkStatus range (HTTP.responseStatus response))
            rspSchemas

      case mbResponseSchema of
        Nothing -> handleResult (UnexpectedStatus completeRequest response)
        Just (_range, schema) -> do
          decodingResult <- parseHTTPResponse schema response
          handleResult $
            ExpectedStatus
              completeRequest
              (removeBody response)
              decodingResult

removeBody :: HTTP.Response a -> HTTP.Response ()
removeBody response =
  response
    { HTTP.responseBody = ()
    }

includeHeadersInRequest :: StatusRange -> Bool
includeHeadersInRequest range =
  case range of
    Status code -> HTTPTypes.statusIsSuccessful (toEnum code)
    Informational -> False
    Success -> True
    Redirect -> False
    ClientError -> False
    ServerError -> False
    AnyStatus -> False
