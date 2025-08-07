{-# LANGUAGE OverloadedStrings #-}

module Test.QuerySchema
  ( tests
  ) where

import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Types as HTTPTypes

import Beeline.Params ((?+))
import qualified Beeline.Params as BP

tests :: IO Bool
tests =
  HH.checkParallel $
    HH.Group
      "QuerySchema"
      [ ("prop_queryParamsRequired", prop_queryParamsRequired)
      , ("prop_queryParamsOptional", prop_queryParamsOptional)
      , ("prop_queryParamsExplodedArray", prop_queryParamsExplodedArray)
      ]

prop_queryParamsRequired :: HH.Property
prop_queryParamsRequired =
  HH.property $ do
    foo <- HH.forAll genText
    bar <- HH.forAll genInt

    let
      fooBarSchema ::
        BP.QuerySchema schema =>
        schema (T.Text, Int) (T.Text, Int)
      fooBarSchema =
        BP.makeParams (,)
          ?+ BP.required fst (BP.textParam "foo")
          ?+ BP.required snd (BP.intParam "bar")

      expectedQuery =
        HTTPTypes.renderQuery
          True
          [ ("foo", Just (Enc.encodeUtf8 foo))
          , ("bar", Just (BS8.pack (show bar)))
          ]

      actualQuery =
        BP.encodeQuery fooBarSchema (foo, bar)

      roundTrippedValue =
        BP.decodeQuery fooBarSchema actualQuery

    expectedQuery === actualQuery
    Right (foo, bar) === roundTrippedValue

prop_queryParamsOptional :: HH.Property
prop_queryParamsOptional =
  HH.property $ do
    foo <- HH.forAll (Gen.maybe genText)
    bar <- HH.forAll (Gen.maybe genInt)

    let
      fooBarSchema ::
        BP.QuerySchema schema =>
        schema (Maybe T.Text, Maybe Int) (Maybe T.Text, Maybe Int)
      fooBarSchema =
        BP.makeParams (,)
          ?+ BP.optional fst (BP.textParam "foo")
          ?+ BP.optional snd (BP.intParam "bar")

      expectedQuery =
        HTTPTypes.renderQuery True
          . filter (isJust . snd)
          $ [ ("foo", fmap Enc.encodeUtf8 foo)
            , ("bar", fmap (BS8.pack . show) bar)
            ]

      actualQuery =
        BP.encodeQuery fooBarSchema (foo, bar)

      roundTrippedValue =
        BP.decodeQuery fooBarSchema actualQuery

    expectedQuery === actualQuery
    Right (foo, bar) === roundTrippedValue

prop_queryParamsExplodedArray :: HH.Property
prop_queryParamsExplodedArray =
  HH.property $ do
    foos <- HH.forAll (Gen.list (Range.linear 0 3) genText)
    bars <- HH.forAll (Gen.list (Range.linear 0 3) genInt)

    let
      fooBarSchema ::
        BP.QuerySchema schema =>
        schema ([T.Text], [Int]) ([T.Text], [Int])
      fooBarSchema =
        BP.makeParams (,)
          ?+ BP.explodedArray fst (BP.textParam "foo")
          ?+ BP.explodedArray snd (BP.intParam "bar")

      mkFooParam :: T.Text -> (BS8.ByteString, Maybe BS8.ByteString)
      mkFooParam foo =
        ("foo", Just (Enc.encodeUtf8 foo))

      mkBarParam :: Int -> (BS8.ByteString, Maybe BS8.ByteString)
      mkBarParam bar =
        ("bar", Just (BS8.pack (show bar)))

      expectedQuery =
        HTTPTypes.renderQuery True $
          fmap mkFooParam foos
            <> fmap mkBarParam bars

      actualQuery =
        BP.encodeQuery fooBarSchema (foos, bars)

      roundTrippedValue =
        BP.decodeQuery fooBarSchema actualQuery

    expectedQuery === actualQuery
    Right (foos, bars) === roundTrippedValue

genText :: HH.Gen T.Text
genText =
  Gen.text (Range.linear 0 32) Gen.unicodeAll

genInt :: HH.Gen Int
genInt =
  Gen.integral (Range.linearFrom 0 minBound maxBound)
