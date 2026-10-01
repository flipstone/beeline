{-# LANGUAGE OverloadedStrings #-}

module Test.CookieSchema
  ( tests
  ) where

import qualified Control.Monad as Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Word as Word
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Web.Cookie as Cookie

import Beeline.Params ((?+))
import qualified Beeline.Params as BP

tests :: IO Bool
tests =
  HH.checkParallel $
    HH.Group
      "CookieSchema"
      [ ("prop_cookiesRequired", prop_cookiesRequired)
      , ("prop_cookiesOptional", prop_cookiesOptional)
      , ("prop_cookieOctetsPassThrough", prop_cookieOctetsPassThrough)
      ]

prop_cookiesRequired :: HH.Property
prop_cookiesRequired =
  HH.property $ do
    foo <- HH.forAll genText
    bar <- HH.forAll genInt

    coverHostileText foo

    let
      fooBarSchema ::
        BP.ParameterSchema schema =>
        schema (T.Text, Int) (T.Text, Int)
      fooBarSchema =
        BP.makeParams (,)
          ?+ BP.required fst (BP.textParam "foo")
          ?+ BP.required snd (BP.intParam "bar")

      actualCookies =
        BP.encodeCookies fooBarSchema (foo, bar)

      roundTrippedValue =
        BP.decodeCookies fooBarSchema actualCookies

    assertValidCookies ["foo", "bar"] actualCookies
    Right (foo, bar) === roundTrippedValue

prop_cookiesOptional :: HH.Property
prop_cookiesOptional =
  HH.property $ do
    foo <- HH.forAll (Gen.maybe genText)
    bar <- HH.forAll (Gen.maybe genInt)

    coverHostileText (fromMaybe "" foo)

    let
      fooBarSchema ::
        BP.ParameterSchema schema =>
        schema (Maybe T.Text, Maybe Int) (Maybe T.Text, Maybe Int)
      fooBarSchema =
        BP.makeParams (,)
          ?+ BP.optional fst (BP.textParam "foo")
          ?+ BP.optional snd (BP.intParam "bar")

      expectedNames =
        catMaybes
          [ "foo" <$ foo
          , "bar" <$ bar
          ]

      actualCookies =
        BP.encodeCookies fooBarSchema (foo, bar)

      roundTrippedValue =
        BP.decodeCookies fooBarSchema actualCookies

    assertValidCookies expectedNames actualCookies
    Right (foo, bar) === roundTrippedValue

prop_cookieOctetsPassThrough :: HH.Property
prop_cookieOctetsPassThrough =
  HH.property $ do
    foo <- HH.forAll genUnambiguousCookieOctets

    HH.cover 10 "foo contains base64 symbols" (T.any (`elem` ("+/=" :: String)) foo)
    HH.cover 10 "foo contains a lone %" (hasLonePercent foo)

    let
      fooSchema ::
        BP.ParameterSchema schema =>
        schema T.Text T.Text
      fooSchema =
        BP.makeParams id
          ?+ BP.required id (BP.textParam "foo")

      expectedCookies =
        LBS.toStrict . BSB.toLazyByteString . Cookie.renderCookies $
          [ ("foo", Enc.encodeUtf8 foo)
          ]

    expectedCookies === BP.encodeCookies fooSchema foo
    Right foo === BP.decodeCookies fooSchema expectedCookies

coverHostileText :: HH.MonadTest m => T.Text -> m ()
coverHostileText text = do
  HH.cover 10 "contains ;" (T.elem ';' text)
  HH.cover 10 "contains a %XX sequence" (hasPercentHexPair text)
  HH.cover 10 "contains a lone %" (hasLonePercent text)

assertValidCookies :: HH.MonadTest m => [BS.ByteString] -> BS.ByteString -> m ()
assertValidCookies expectedNames encodedCookies = do
  let
    parsedCookies = Cookie.parseCookies encodedCookies

  expectedNames === fmap fst parsedCookies
  HH.assert $ all (BS.all isCookieOctet . snd) parsedCookies

isCookieOctet :: Word.Word8 -> Bool
isCookieOctet byte =
  byte == 0x21
    || (byte >= 0x23 && byte <= 0x2B)
    || (byte >= 0x2D && byte <= 0x3A)
    || (byte >= 0x3C && byte <= 0x5B)
    || (byte >= 0x5D && byte <= 0x7E)

hasPercentHexPair :: T.Text -> Bool
hasPercentHexPair =
  any startsWithPercentHexPair . List.tails . T.unpack

hasLonePercent :: T.Text -> Bool
hasLonePercent =
  let
    isLonePercent chars =
      case chars of
        '%' : _ -> not (startsWithPercentHexPair chars)
        _ -> False
  in
    any isLonePercent . List.tails . T.unpack

startsWithPercentHexPair :: String -> Bool
startsWithPercentHexPair chars =
  case chars of
    '%' : first : second : _ -> Char.isHexDigit first && Char.isHexDigit second
    _ -> False

genText :: HH.Gen T.Text
genText =
  fmap T.concat . Gen.list (Range.linear 0 16) $
    Gen.frequency
      [ (5, T.singleton <$> Gen.unicodeAll)
      , (2, T.singleton <$> Gen.element ("=, \"\\+" :: String))
      , (1, pure ";")
      , (1, pure "%")
      , (1, T.pack . ('%' :) <$> Monad.replicateM 2 Gen.hexit)
      ]

genUnambiguousCookieOctets :: HH.Gen T.Text
genUnambiguousCookieOctets =
  let
    octetChars =
      filter (isCookieOctet . fromIntegral . Char.ord) ['\x21' .. '\x7E']

    nonPercentChars =
      filter (/= '%') octetChars

    nonHexChars =
      filter (not . Char.isHexDigit) nonPercentChars
  in
    fmap T.concat . Gen.list (Range.linear 0 16) $
      Gen.frequency
        [ (6, T.singleton <$> Gen.element nonPercentChars)
        , (1, T.singleton <$> Gen.element ("+/=" :: String))
        , (1, T.pack . ('%' :) . pure <$> Gen.element nonHexChars)
        ]

genInt :: HH.Gen Int
genInt =
  Gen.integral (Range.linearFrom 0 minBound maxBound)
