{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exc
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IORef as IORef
import Data.Maybe (isJust)
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

import Beeline.HTTP.Client ((?+))
import qualified Beeline.HTTP.Client as BHC
import Beeline.Routing ((/+), (/-))
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
  , ("prop_httpMultiResponse", prop_httpMultiResponse)
  , ("prop_httpUnexpectedStatus", prop_httpUnexpectedStatus)
  , ("prop_httpDecodingFailure", prop_httpDecodingFailure)
  , ("prop_httpGetQueryParams", prop_httpGetQueryParams)
  , ("prop_httpPostQueryParams", prop_httpPostQueryParams)
  , ("prop_queryParamsRequired", prop_queryParamsRequired)
  , ("prop_queryParamsOptional", prop_queryParamsOptional)
  , ("prop_queryParamsExplodedArray", prop_queryParamsExplodedArray)
  , ("prop_basePath", prop_basePath)
  , ("prop_headers", prop_headers)
  ]

newtype FooBarId
  = FooBarId Int

data GetFooBar = GetFooBar
  { fooBarId :: FooBarId
  }

fooBarIdParam :: R.ParameterDefinition FooBarId
fooBarIdParam =
  R.coerceParam (R.intParam "fooBarId")

getFooBar ::
  BHC.Operation
    BHC.ContentTypeDecodingError
    GetFooBar
    BHC.NoQueryParams
    BHC.NoRequestBody
    T.Text
getFooBar =
  BHC.defaultOperation
    { BHC.requestRoute =
        R.get $
          R.make GetFooBar
            /- "foobars"
            /+ R.Param fooBarIdParam fooBarId
    , BHC.responseSchemas =
        [ (BHC.Success, BHC.responseBody BHC.PlainText BHC.UTF8)
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
            request =
              BHC.defaultRequest
                { BHC.baseURI = BHC.defaultBaseURI {BHC.port = port}
                , BHC.route = GetFooBar (FooBarId 1)
                }

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          BHC.httpRequestThrow getFooBar request manager

      response <- HH.evalIO (withTestServer handleRequest issueRequest)
      response === expectedResponse

postText ::
  BHC.Operation
    BHC.ContentTypeDecodingError
    BHC.NoPathParams
    BHC.NoQueryParams
    T.Text
    T.Text
postText =
  BHC.defaultOperation
    { BHC.requestRoute = R.post (R.make BHC.NoPathParams)
    , BHC.requestBodySchema = BHC.requestBody BHC.PlainText BHC.UTF8
    , BHC.responseSchemas =
        [ (BHC.Success, BHC.responseBody BHC.PlainText BHC.UTF8)
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
            request =
              BHC.defaultRequest
                { BHC.baseURI = BHC.defaultBaseURI {BHC.port = port}
                , BHC.body = expectedBody
                }

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          BHC.httpRequestThrow postText request manager

      response <- HH.evalIO (withTestServer handleRequest issueRequest)
      response === expectedResponse

postNoResponse ::
  BHC.Operation
    BHC.ContentTypeDecodingError
    BHC.NoPathParams
    BHC.NoQueryParams
    T.Text
    BHC.NoResponseBody
postNoResponse =
  BHC.defaultOperation
    { BHC.requestRoute = R.post (R.make BHC.NoPathParams)
    , BHC.requestBodySchema = BHC.requestBody BHC.PlainText BHC.UTF8
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
            request =
              BHC.defaultRequest
                { BHC.baseURI = BHC.defaultBaseURI {BHC.port = port}
                , BHC.body = expectedRequestBody
                }

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          BHC.httpRequestThrow postNoResponse request manager

      response <- HH.evalIO (withTestServer handleRequest issueRequest)
      response === BHC.NoResponseBody

postBytes ::
  BHC.Operation
    BHC.ContentTypeDecodingError
    BHC.NoPathParams
    BHC.NoQueryParams
    BS.ByteString
    BS.ByteString
postBytes =
  BHC.defaultOperation
    { BHC.requestRoute = R.post (R.make BHC.NoPathParams)
    , BHC.requestBodySchema = BHC.requestBody BHC.OctetStream BHC.Bytes
    , BHC.responseSchemas =
        [ (BHC.Success, BHC.responseBody BHC.OctetStream BHC.Bytes)
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
            request =
              BHC.defaultRequest
                { BHC.baseURI = BHC.defaultBaseURI {BHC.port = port}
                , BHC.body = expectedBody
                }

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          BHC.httpRequestThrow postBytes request manager

      response <- HH.evalIO (withTestServer handleRequest issueRequest)
      response === expectedResponse

data MultiStatus
  = Multi200 T.Text
  | MultiOtherSuccess T.Text
  | MultiClientError T.Text
  deriving (Show, Eq)

multipleResponseCodes ::
  BHC.Operation
    BHC.ContentTypeDecodingError
    BHC.NoPathParams
    BHC.NoQueryParams
    BHC.NoRequestBody
    MultiStatus
multipleResponseCodes =
  BHC.defaultOperation
    { BHC.responseSchemas =
        [ (BHC.Status 200, fmap Multi200 (BHC.responseBody BHC.PlainText BHC.UTF8))
        , (BHC.Success, fmap MultiOtherSuccess (BHC.responseBody BHC.PlainText BHC.UTF8))
        , (BHC.ClientError, fmap MultiClientError (BHC.responseBody BHC.PlainText BHC.UTF8))
        ]
    }

prop_httpMultiResponse :: HH.Property
prop_httpMultiResponse =
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
          request =
            BHC.defaultRequest
              { BHC.baseURI = BHC.defaultBaseURI {BHC.port = port}
              }

        manager <- HTTP.newManager HTTP.defaultManagerSettings
        BHC.httpRequestThrow multipleResponseCodes request manager

    response <- HH.evalIO (withTestServer handleRequest issueRequest)
    response === expectedResponse

prop_httpUnexpectedStatus :: HH.Property
prop_httpUnexpectedStatus =
  HH.withTests 1 . HH.property $ do
    let
      handleRequest _request =
        pure $ Wai.responseLBS HTTPTypes.notFound404 [] "Not Found"

      issueRequest port = do
        let
          request =
            BHC.defaultRequest
              { BHC.baseURI = BHC.defaultBaseURI {BHC.port = port}
              , BHC.route = GetFooBar (FooBarId 1)
              }

        manager <- HTTP.newManager HTTP.defaultManagerSettings
        Exc.try $ BHC.httpRequestThrow getFooBar request manager

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

prop_httpDecodingFailure :: HH.Property
prop_httpDecodingFailure =
  HH.withTests 1 . HH.property $ do
    let
      handleRequest _request =
        pure $ Wai.responseLBS HTTPTypes.ok200 [] "\xfc\xa1\xa1\xa1\xa1\xa1"

      issueRequest port = do
        let
          request =
            BHC.defaultRequest
              { BHC.baseURI = BHC.defaultBaseURI {BHC.port = port}
              , BHC.route = GetFooBar (FooBarId 1)
              }

        manager <- HTTP.newManager HTTP.defaultManagerSettings
        Exc.try $ BHC.httpRequestThrow getFooBar request manager

    exceptionOrFooBar <- HH.evalIO (withTestServer handleRequest issueRequest)
    case exceptionOrFooBar of
      Left (BHC.ContentTypeDecodingError msg) ->
        msg === "Cannot decode byte '\\xfc': Data.Text.Internal.Encoding.streamDecodeUtf8With: Invalid UTF-8 stream"
      Right _fooBar -> do
        HH.annotate "Expected an HTTPException, but got a valid response instead"
        HH.failure

data TestQueryParams = TestQueryParams
  { queryParam1 :: T.Text
  , queryParam2 :: Maybe Int
  }
  deriving (Show, Eq)

testQueryParamSchema :: BHC.QuerySchema q => q TestQueryParams TestQueryParams
testQueryParamSchema =
  BHC.makeQuery TestQueryParams
    ?+ BHC.required queryParam1 (R.textParam "param1")
    ?+ BHC.optional queryParam2 (R.intParam "param2")

getQueryParams ::
  BHC.Operation
    BHC.ContentTypeDecodingError
    BHC.NoPathParams
    TestQueryParams
    BHC.NoRequestBody
    BHC.NoResponseBody
getQueryParams =
  BHC.defaultOperation
    { BHC.requestQuerySchema = testQueryParamSchema
    }

prop_httpGetQueryParams :: HH.Property
prop_httpGetQueryParams =
  HH.withTests 1 . HH.property $ do
    withAssertLater $ \assertLater -> do
      let
        expectedParams =
          TestQueryParams
            { queryParam1 = "value"
            , queryParam2 = Just 32
            }

        handleRequest request = do
          assertLater $ do
            Right expectedParams
              === BHC.decodeQuery
                testQueryParamSchema
                (Wai.rawQueryString request)

          pure . responseText HTTPTypes.ok200 $ ""

        issueRequest port = do
          let
            request =
              BHC.defaultRequest
                { BHC.baseURI = BHC.defaultBaseURI {BHC.port = port}
                , BHC.query = expectedParams
                }

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          BHC.httpRequestThrow getQueryParams request manager

      response <- HH.evalIO (withTestServer handleRequest issueRequest)
      response === BHC.NoResponseBody

postQueryParams ::
  BHC.Operation
    BHC.ContentTypeDecodingError
    BHC.NoPathParams
    BHC.NoQueryParams
    TestQueryParams
    BHC.NoResponseBody
postQueryParams =
  BHC.defaultOperation
    { BHC.requestBodySchema =
        BHC.requestBody BHC.FormURLEncoded (BHC.FormEncoder testQueryParamSchema)
    }

prop_httpPostQueryParams :: HH.Property
prop_httpPostQueryParams =
  HH.withTests 1 . HH.property $ do
    withAssertLater $ \assertLater -> do
      let
        expectedParams =
          TestQueryParams
            { queryParam1 = "value"
            , queryParam2 = Just 32
            }

        handleRequest request = do
          body <- fmap LBS.toStrict (Wai.consumeRequestBodyStrict request)
          assertLater $ do
            lookup "Content-Type" (Wai.requestHeaders request) === Just "application/x-www-form-urlencoded"
            Right expectedParams === BHC.decodeQuery testQueryParamSchema body

          pure . responseText HTTPTypes.ok200 $ ""

        issueRequest port = do
          let
            request =
              BHC.defaultRequest
                { BHC.baseURI = BHC.defaultBaseURI {BHC.port = port}
                , BHC.body = expectedParams
                }

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          BHC.httpRequestThrow postQueryParams request manager

      response <- HH.evalIO (withTestServer handleRequest issueRequest)
      response === BHC.NoResponseBody

requiredFooBarQuery ::
  BHC.QuerySchema r =>
  r (T.Text, Int) (T.Text, Int)
requiredFooBarQuery =
  BHC.makeQuery (,)
    ?+ BHC.required fst (R.textParam "foo")
    ?+ BHC.required snd (R.intParam "bar")

prop_queryParamsRequired :: HH.Property
prop_queryParamsRequired =
  HH.property $ do
    foo <- HH.forAll genText
    bar <- HH.forAll genInt

    let
      expectedQuery =
        HTTPTypes.renderQuery True $
          [ ("foo", Just (Enc.encodeUtf8 foo))
          , ("bar", Just (BS8.pack (show bar)))
          ]

      actualQuery =
        BHC.encodeQuery requiredFooBarQuery (foo, bar)

      roundTrippedValue =
        BHC.decodeQuery requiredFooBarQuery actualQuery

    expectedQuery === actualQuery
    Right (foo, bar) === roundTrippedValue

optionalFooBarQuery ::
  BHC.QuerySchema r =>
  r (Maybe T.Text, Maybe Int) (Maybe T.Text, Maybe Int)
optionalFooBarQuery =
  BHC.makeQuery (,)
    ?+ BHC.optional fst (R.textParam "foo")
    ?+ BHC.optional snd (R.intParam "bar")

prop_queryParamsOptional :: HH.Property
prop_queryParamsOptional =
  HH.property $ do
    foo <- HH.forAll (Gen.maybe genText)
    bar <- HH.forAll (Gen.maybe genInt)

    let
      expectedQuery =
        HTTPTypes.renderQuery True $
          filter (isJust . snd) $
            [ ("foo", fmap Enc.encodeUtf8 foo)
            , ("bar", fmap (BS8.pack . show) bar)
            ]

      actualQuery =
        BHC.encodeQuery optionalFooBarQuery (foo, bar)

      roundTrippedValue =
        BHC.decodeQuery optionalFooBarQuery actualQuery

    expectedQuery === actualQuery
    Right (foo, bar) === roundTrippedValue

explodedArrayFooBarQuery ::
  BHC.QuerySchema r =>
  r ([T.Text], [Int]) ([T.Text], [Int])
explodedArrayFooBarQuery =
  BHC.makeQuery (,)
    ?+ BHC.explodedArray fst (R.textParam "foo")
    ?+ BHC.explodedArray snd (R.intParam "bar")

prop_queryParamsExplodedArray :: HH.Property
prop_queryParamsExplodedArray =
  HH.property $ do
    foos <- HH.forAll (Gen.list (Range.linear 0 3) genText)
    bars <- HH.forAll (Gen.list (Range.linear 0 3) genInt)

    let
      mkFooParam foo =
        ("foo", Just (Enc.encodeUtf8 foo))

      mkBarParam bar =
        ("bar", Just (BS8.pack (show bar)))

      expectedQuery =
        HTTPTypes.renderQuery True $
          fmap mkFooParam foos
            <> fmap mkBarParam bars

      actualQuery =
        BHC.encodeQuery explodedArrayFooBarQuery (foos, bars)

      roundTrippedValue =
        BHC.decodeQuery explodedArrayFooBarQuery actualQuery

    expectedQuery === actualQuery
    Right (foos, bars) === roundTrippedValue

prop_basePath :: HH.Property
prop_basePath =
  HH.withTests 1 . HH.property $ do
    withAssertLater $ \assertLater -> do
      let
        handleRequest request = do
          assertLater $ do
            Wai.pathInfo request === ["someBasePath", "foobars", "1"]

          pure . responseText HTTPTypes.ok200 $ ""

        issueRequest port = do
          let
            request =
              BHC.defaultRequest
                { BHC.baseURI =
                    BHC.defaultBaseURI
                      { BHC.port = port
                      , BHC.basePath = "/someBasePath"
                      }
                , BHC.route = GetFooBar (FooBarId 1)
                }

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          void $ BHC.httpRequestThrow getFooBar request manager

      HH.evalIO (withTestServer handleRequest issueRequest)

prop_headers :: HH.Property
prop_headers =
  HH.withTests 1 . HH.property $ do
    withAssertLater $ \assertLater -> do
      let
        handleRequest request = do
          assertLater $ do
            lookup "Some-Header" (Wai.requestHeaders request) === Just "foobar"

          pure . responseText HTTPTypes.ok200 $ ""

        issueRequest port = do
          let
            request =
              BHC.defaultRequest
                { BHC.baseURI = BHC.defaultBaseURI {BHC.port = port}
                , BHC.route = GetFooBar (FooBarId 1)
                , BHC.headers = [("Some-Header", "foobar")]
                }

          manager <- HTTP.newManager HTTP.defaultManagerSettings
          void $ BHC.httpRequestThrow getFooBar request manager

      HH.evalIO (withTestServer handleRequest issueRequest)

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

genInt :: HH.Gen Int
genInt =
  Gen.integral (Range.linearFrom 0 minBound maxBound)
