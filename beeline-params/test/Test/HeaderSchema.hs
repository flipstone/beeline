{-# LANGUAGE OverloadedStrings #-}

module Test.HeaderSchema
  ( tests
  ) where

import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Beeline.Params ((?+))
import qualified Beeline.Params as BP

tests :: IO Bool
tests =
  HH.checkParallel $
    HH.Group
      "HeaderSchema"
      [ ("prop_headersRequired", prop_headersRequired)
      , ("prop_headersOptional", prop_headersOptional)
      , ("prop_headersCookies", prop_headersCookies)
      ]

prop_headersRequired :: HH.Property
prop_headersRequired =
  HH.property $ do
    foo <- HH.forAll genText
    bar <- HH.forAll genInt

    let
      fooBarSchema ::
        BP.HeaderSchema schema =>
        schema (T.Text, Int) (T.Text, Int)
      fooBarSchema =
        BP.makeParams (,)
          ?+ BP.required fst (BP.textParam "foo")
          ?+ BP.required snd (BP.intParam "bar")

      expectedHeaders =
        [ ("foo", Enc.encodeUtf8 foo)
        , ("bar", BS8.pack (show bar))
        ]

      actualHeaders =
        BP.encodeHeaders fooBarSchema (foo, bar)

      roundTrippedValue =
        BP.decodeHeaders fooBarSchema actualHeaders

    expectedHeaders === actualHeaders
    Right (foo, bar) === roundTrippedValue

prop_headersOptional :: HH.Property
prop_headersOptional =
  HH.property $ do
    foo <- HH.forAll (Gen.maybe genText)
    bar <- HH.forAll (Gen.maybe genInt)

    let
      fooBarSchema ::
        BP.HeaderSchema schema =>
        schema (Maybe T.Text, Maybe Int) (Maybe T.Text, Maybe Int)
      fooBarSchema =
        BP.makeParams (,)
          ?+ BP.optional fst (BP.textParam "foo")
          ?+ BP.optional snd (BP.intParam "bar")

      mbTuple :: a -> Maybe b -> Maybe (a, b)
      mbTuple k mbV =
        case mbV of
          Nothing -> Nothing
          Just v -> Just (k, v)

      expectedHeaders =
        catMaybes
          [ mbTuple "foo" (fmap Enc.encodeUtf8 foo)
          , mbTuple "bar" (fmap (BS8.pack . show) bar)
          ]

      actualHeaders =
        BP.encodeHeaders fooBarSchema (foo, bar)

      roundTrippedValue =
        BP.decodeHeaders fooBarSchema actualHeaders

    expectedHeaders === actualHeaders
    Right (foo, bar) === roundTrippedValue

prop_headersCookies :: HH.Property
prop_headersCookies =
  HH.property $ do
    foo <- HH.forAll genText
    bar <- HH.forAll genInt

    let
      cookieSchema ::
        BP.ParameterSchema schema =>
        schema (T.Text, Int) (T.Text, Int)
      cookieSchema =
        BP.makeParams (,)
          ?+ BP.required fst (BP.textParam "foo")
          ?+ BP.required snd (BP.intParam "bar")

      headerSchema ::
        BP.HeaderSchema schema =>
        schema (T.Text, Int) (T.Text, Int)
      headerSchema =
        BP.makeParams id
          ?+ BP.cookies id cookieSchema

      expectedHeaders =
        [ ("Cookie", BP.encodeCookies cookieSchema (foo, bar))
        ]

      actualHeaders =
        BP.encodeHeaders headerSchema (foo, bar)

      roundTrippedValue =
        BP.decodeHeaders headerSchema actualHeaders

    expectedHeaders === actualHeaders
    Right (foo, bar) === roundTrippedValue

genText :: HH.Gen T.Text
genText =
  Gen.text (Range.linear 0 32) Gen.unicodeAll

genInt :: HH.Gen Int
genInt =
  Gen.integral (Range.linearFrom 0 minBound maxBound)
