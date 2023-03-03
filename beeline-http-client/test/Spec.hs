{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exc
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IORef as IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as HHM
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Random as Rand

import qualified Beeline.HTTP.Client as BHC
import qualified Beeline.Routing as R

main :: IO ()
main =
  HHM.defaultMain $
    [ HH.checkSequential (HH.Group "beeline-http-client" tests)
    ]

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_httpGet", prop_httpGet)
  , ("prop_httpPostText", prop_httpPostText)
  , ("prop_httpPostNoResponse", prop_httpPostNoResponse)
  , ("prop_httpPostBytes", prop_httpPostBytes)
  , ("prop_multiResponse", prop_multiResponse)
  , ("prop_unexpectedStatus", prop_unexpectedStatus)
  , ("prop_decodingFailure", prop_decodingFailure)
  ]

newtype FooBarId
  = FooBarId Int

fooBarIdParam :: R.ParameterDefinition FooBarId
fooBarIdParam =
  R.coerceParam (R.integralParam "fooBarId" :: R.ParameterDefinition Int)

getFooBar ::
  BHC.RequestDefinition
    BHC.ContentTypeDecodingError
    FooBarId
    ()
    T.Text
getFooBar =
  BHC.defaultRequestDefinition
    { BHC.requestRoute =
        R.route id
          . R.piece "foobars"
          . R.param fooBarIdParam id
          . R.end
          $ HTTPTypes.GET
    , BHC.responseSchemas =
        [ (BHC.Success, BHC.decodeResponseWith BHC.PlainText BHC.UTF8)
        ]
    }

prop_httpGet :: HH.Property
prop_httpGet =
  HH.withTests 1 . HH.property $ do
    withAssertLater $ \assertLater -> do
      let
        expectedResponse =
          "response body"

        handleRequest request = do
          body <- Wai.consumeRequestBodyStrict request

          assertLater $ do
            Wai.requestMethod request === HTTPTypes.methodGet
            Wai.pathInfo request === ["foobars", "1"]
            lookup "Content-Type" (Wai.requestHeaders request) === Nothing
            lookup "Accept" (Wai.requestHeaders request) === Just "text/plain; charset=utf8"
            body === ""

          pure . responseText HTTPTypes.ok200 $ expectedResponse

        issueRequest port = do
          let
            request = HTTP.defaultRequest {HTTP.port = port}

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          BHC.httpRequestThrow getFooBar (FooBarId 1) () request manager

      response <- HH.evalIO (withTestServer handleRequest issueRequest)
      response === expectedResponse

postText ::
  BHC.RequestDefinition
    BHC.ContentTypeDecodingError
    ()
    T.Text
    T.Text
postText =
  BHC.defaultRequestDefinition
    { BHC.requestRoute = R.route () $ R.end HTTPTypes.POST
    , BHC.requestSchema = BHC.encodeRequestWith BHC.PlainText BHC.UTF8
    , BHC.responseSchemas =
        [ (BHC.Success, BHC.decodeResponseWith BHC.PlainText BHC.UTF8)
        ]
    }

prop_httpPostText :: HH.Property
prop_httpPostText =
  HH.withTests 1 . HH.property $ do
    withAssertLater $ \assertLater -> do
      let
        expectedBody =
          "request body"

        expectedResponse =
          "response body"

        handleRequest request = do
          body <- Wai.consumeRequestBodyStrict request

          assertLater $ do
            Wai.requestMethod request === HTTPTypes.methodPost
            lookup "Content-Type" (Wai.requestHeaders request) === Just "text/plain; charset=utf8"
            lookup "Content-Length" (Wai.requestHeaders request) === Just "12"
            lookup "Accept" (Wai.requestHeaders request) === Just "text/plain; charset=utf8"
            Enc.decodeUtf8' (LBS.toStrict body) === Right expectedBody

          pure . responseText HTTPTypes.ok200 $ expectedResponse

        issueRequest port = do
          let
            request = HTTP.defaultRequest {HTTP.port = port}

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          BHC.httpRequestThrow postText () expectedBody request manager

      response <- HH.evalIO (withTestServer handleRequest issueRequest)
      response === expectedResponse

postNoResponse ::
  BHC.RequestDefinition
    BHC.ContentTypeDecodingError
    ()
    T.Text
    ()
postNoResponse =
  BHC.defaultRequestDefinition
    { BHC.requestRoute = R.route () $ R.end HTTPTypes.POST
    , BHC.requestSchema = BHC.encodeRequestWith BHC.PlainText BHC.UTF8
    }

prop_httpPostNoResponse :: HH.Property
prop_httpPostNoResponse =
  HH.withTests 1 . HH.property $ do
    withAssertLater $ \assertLater -> do
      let
        expectedRequestBody =
          "request body"

        handleRequest request = do
          body <- Wai.consumeRequestBodyStrict request

          assertLater $ do
            Wai.requestMethod request === HTTPTypes.methodPost
            lookup "Content-Type" (Wai.requestHeaders request) === Just "text/plain; charset=utf8"
            lookup "Content-Length" (Wai.requestHeaders request) === Just "12"
            lookup "Accept" (Wai.requestHeaders request) === Nothing
            Enc.decodeUtf8' (LBS.toStrict body) === Right expectedRequestBody

          pure $ Wai.responseLBS HTTPTypes.ok200 [] ""

        issueRequest port = do
          let
            request = HTTP.defaultRequest {HTTP.port = port}

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          BHC.httpRequestThrow postNoResponse () expectedRequestBody request manager

      response <- HH.evalIO (withTestServer handleRequest issueRequest)
      response === ()

postBytes ::
  BHC.RequestDefinition
    BHC.ContentTypeDecodingError
    ()
    BS.ByteString
    BS.ByteString
postBytes =
  BHC.defaultRequestDefinition
    { BHC.requestRoute = R.route () $ R.end HTTPTypes.POST
    , BHC.requestSchema = BHC.encodeRequestWith BHC.OctetStream BHC.Bytes
    , BHC.responseSchemas =
        [ (BHC.Success, BHC.decodeResponseWith BHC.OctetStream BHC.Bytes)
        ]
    }

prop_httpPostBytes :: HH.Property
prop_httpPostBytes =
  HH.withTests 1 . HH.property $ do
    withAssertLater $ \assertLater -> do
      let
        expectedBody =
          "request body"

        expectedResponse =
          "response body"

        handleRequest request = do
          body <- Wai.consumeRequestBodyStrict request

          assertLater $ do
            Wai.requestMethod request === HTTPTypes.methodPost
            lookup "Content-Type" (Wai.requestHeaders request) === Just "application/octet-stream"
            lookup "Content-Length" (Wai.requestHeaders request) === Just "12"
            lookup "Accept" (Wai.requestHeaders request) === Just "application/octet-stream"
            LBS.toStrict body === expectedBody

          pure
            . Wai.responseLBS HTTPTypes.ok200 []
            . LBS.fromStrict
            $ expectedResponse

        issueRequest port = do
          let
            request = HTTP.defaultRequest {HTTP.port = port}

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          BHC.httpRequestThrow postBytes () expectedBody request manager

      response <- HH.evalIO (withTestServer handleRequest issueRequest)
      response === expectedResponse

data MultiStatus
  = Multi200 T.Text
  | MultiOtherSuccess T.Text
  | MultiClientError T.Text
  deriving (Show, Eq)

multipleResponseCodes ::
  BHC.RequestDefinition
    BHC.ContentTypeDecodingError
    ()
    ()
    MultiStatus
multipleResponseCodes =
  BHC.defaultRequestDefinition
    { BHC.responseSchemas =
        [ (BHC.Status 200, fmap Multi200 (BHC.decodeResponseWith BHC.PlainText BHC.UTF8))
        , (BHC.Success, fmap MultiOtherSuccess (BHC.decodeResponseWith BHC.PlainText BHC.UTF8))
        , (BHC.ClientError, fmap MultiClientError (BHC.decodeResponseWith BHC.PlainText BHC.UTF8))
        ]
    }

prop_multiResponse :: HH.Property
prop_multiResponse =
  HH.property $ do
    expectedResponse <-
      HH.forAll $
        Gen.choice
          [ fmap Multi200 genText
          , fmap MultiOtherSuccess genText
          , fmap MultiClientError genText
          ]

    let
      handleRequest _request =
        pure $
          case expectedResponse of
            Multi200 foo ->
              responseText HTTPTypes.ok200 foo
            MultiOtherSuccess bar ->
              responseText HTTPTypes.created201 bar
            MultiClientError baz ->
              responseText HTTPTypes.notFound404 baz

      issueRequest port = do
        let
          request = HTTP.defaultRequest {HTTP.port = port}

        manager <- HTTP.newManager HTTP.defaultManagerSettings
        BHC.httpRequestThrow multipleResponseCodes () () request manager

    response <- HH.evalIO (withTestServer handleRequest issueRequest)
    response === expectedResponse

prop_unexpectedStatus :: HH.Property
prop_unexpectedStatus =
  HH.withTests 1 . HH.property $ do
    let
      handleRequest _request =
        pure $ Wai.responseLBS HTTPTypes.notFound404 [] "Not Found"

      issueRequest port = do
        let
          request = HTTP.defaultRequest {HTTP.port = port}

        manager <- HTTP.newManager HTTP.defaultManagerSettings
        Exc.try $ BHC.httpRequestThrow getFooBar (FooBarId 1) () request manager

    exceptionOrFooBar <- HH.evalIO (withTestServer handleRequest issueRequest)
    case exceptionOrFooBar of
      Left (HTTP.HttpExceptionRequest _request (HTTP.StatusCodeException _response chunk)) ->
        chunk === "Not Found"
      Left err -> do
        HH.annotate "Expected a StatusCodeException, but got:"
        HH.annotateShow err
        HH.failure
      Right _fooBar -> do
        HH.annotate "Expected an HTTPException, but got a valid response instead"
        HH.failure

prop_decodingFailure :: HH.Property
prop_decodingFailure =
  HH.withTests 1 . HH.property $ do
    let
      handleRequest _request =
        pure $ Wai.responseLBS HTTPTypes.ok200 [] "\xfc\xa1\xa1\xa1\xa1\xa1"

      issueRequest port = do
        let
          request = HTTP.defaultRequest {HTTP.port = port}

        manager <- HTTP.newManager HTTP.defaultManagerSettings
        Exc.try $ BHC.httpRequestThrow getFooBar (FooBarId 1) () request manager

    exceptionOrFooBar <- HH.evalIO (withTestServer handleRequest issueRequest)
    case exceptionOrFooBar of
      Left (BHC.ContentTypeDecodingError msg) ->
        msg === "Cannot decode byte '\\xfc': Data.Text.Internal.Encoding.streamDecodeUtf8With: Invalid UTF-8 stream"
      Right _fooBar -> do
        HH.annotate "Expected an HTTPException, but got a valid response instead"
        HH.failure

withAssertLater ::
  ((HH.PropertyT IO () -> IO ()) -> HH.PropertyT IO a) ->
  HH.PropertyT IO a
withAssertLater action = do
  serverAssertions <- HH.evalIO (IORef.newIORef [])

  let
    assertLater assertion =
      IORef.modifyIORef serverAssertions (assertion :)

  result <- action assertLater
  assertions <- HH.evalIO (IORef.readIORef serverAssertions)
  sequence_ (reverse assertions)
  pure result

withTestServer :: (Wai.Request -> IO Wai.Response) -> (Warp.Port -> IO a) -> IO a
withTestServer handleRequest issueRequest = do
  startupVar <- MVar.newEmptyMVar
  port <- Rand.randomRIO (10000, 50000)

  let
    serverSettings =
      Warp.setPort port
        . Warp.setBeforeMainLoop (MVar.putMVar startupVar ())
        $ Warp.defaultSettings

    server =
      Warp.runSettings serverSettings $ \request respondWith ->
        respondWith =<< handleRequest request

    waitForServer =
      MVar.readMVar startupVar

  Exc.bracket
    (Conc.forkIO server)
    Conc.killThread
    (\_threadId -> waitForServer >> issueRequest port)

responseText :: HTTPTypes.Status -> T.Text -> Wai.Response
responseText status =
  Wai.responseLBS status []
    . LBS.fromStrict
    . Enc.encodeUtf8

genText :: HH.Gen T.Text
genText =
  Gen.text (Range.linear 0 32) Gen.unicodeAll
