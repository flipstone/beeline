{-# LANGUAGE TypeFamilies #-}

{- |
Copyright : Flipstone Technology Partners 2025
License   : MIT

@since 0.1.0.0
-}
module Beeline.Params.CookieSchema
  ( CookieEncoder (..)
  , CookieBuilder
  , encodeCookies
  , CookieDecoder (..)
  , CookieMap
  , decodeCookies
  ) where

import Control.Monad ((<=<))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.DList as DList
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Word as Word
import qualified Network.HTTP.Types as HTTPTypes
import qualified Web.Cookie as Cookie

import qualified Beeline.Params.Internal.ParamLookup as PL
import qualified Beeline.Params.ParameterDefinition as PD
import qualified Beeline.Params.ParameterSchema as PS

{- |
  An implementation of ParameterSchema for encoding queries
-}
newtype CookieEncoder a b
  = CookieEncoder (a -> CookieBuilder)

type CookieBuilder =
  DList.DList (BS.ByteString, BS.ByteString)

{- |
  Render a value to a list of HTTP headers.
-}
encodeCookies :: CookieEncoder a b -> a -> BS.ByteString
encodeCookies (CookieEncoder f) =
  LBS.toStrict . BSB.toLazyByteString . Cookie.renderCookies . DList.toList . f

instance PS.ParameterSchema CookieEncoder where
  newtype Parameter CookieEncoder record _a
    = EncodeCookies (record -> CookieBuilder)

  makeParams _constructor =
    CookieEncoder (const mempty)

  validateParams unvalidate _validate (CookieEncoder f) =
    CookieEncoder (f . unvalidate)

  addParam (CookieEncoder f) (EncodeCookies g) =
    CookieEncoder $ \a -> f a <> g a

  required accessor paramDef =
    EncodeCookies (encodeCookie paramDef . accessor)

  optional accessor paramDef =
    EncodeCookies $ \record ->
      case accessor record of
        Nothing -> mempty
        Just param -> encodeCookie paramDef param

  splat accessor (CookieEncoder f) =
    EncodeCookies (f . accessor)

encodeCookie :: PD.ParameterDefinition param -> param -> CookieBuilder
encodeCookie paramDef value =
  let
    encodedName =
      Enc.encodeUtf8 (PD.parameterName paramDef)

    encodedValue =
      encodeCookieValue . Enc.encodeUtf8 $ PD.parameterRenderer paramDef value
  in
    DList.singleton (encodedName, encodedValue)

-- Percent-encodes only the bytes that are not valid cookie-octets as defined
-- in https://www.rfc-editor.org/rfc/rfc6265#section-4.1.1, plus any '%' that
-- would otherwise be read back as the start of a percent-encoded byte. Other
-- valid values are sent unchanged.
encodeCookieValue :: BS.ByteString -> BS.ByteString
encodeCookieValue bytes =
  let
    (plain, rest) = BS.span isPlainCookieOctet bytes
  in
    if BS.null rest
      then bytes
      else LBS.toStrict . BSB.toLazyByteString $ BSB.byteString plain <> encodeFromSpecialByte rest

encodeFromSpecialByte :: BS.ByteString -> BSB.Builder
encodeFromSpecialByte bytes =
  case BS.uncons bytes of
    Nothing -> mempty
    Just (byte, remaining) ->
      let
        (plain, rest) = BS.span isPlainCookieOctet remaining
      in
        encodeSpecialByte byte remaining <> BSB.byteString plain <> encodeFromSpecialByte rest

encodeSpecialByte :: Word.Word8 -> BS.ByteString -> BSB.Builder
encodeSpecialByte byte remaining =
  if byte == percentSign && not (startsWithHexPair remaining)
    then BSB.word8 byte
    else percentEscape byte

isPlainCookieOctet :: Word.Word8 -> Bool
isPlainCookieOctet byte =
  byte /= percentSign && isCookieOctet byte

percentEscape :: Word.Word8 -> BSB.Builder
percentEscape byte =
  let
    (high, low) = byte `quotRem` 16

    hexDigit n =
      if n < 10
        then BSB.word8 (48 + n)
        else BSB.word8 (55 + n)
  in
    BSB.word8 percentSign <> hexDigit high <> hexDigit low

startsWithHexPair :: BS.ByteString -> Bool
startsWithHexPair bytes =
  case BS.uncons bytes of
    Just (first, rest) | isHexDigit first -> maybe False (isHexDigit . fst) (BS.uncons rest)
    _ -> False

isHexDigit :: Word.Word8 -> Bool
isHexDigit byte =
  (byte >= 0x30 && byte <= 0x39)
    || (byte >= 0x41 && byte <= 0x46)
    || (byte >= 0x61 && byte <= 0x66)

isCookieOctet :: Word.Word8 -> Bool
isCookieOctet byte =
  byte == 0x21
    || (byte >= 0x23 && byte <= 0x2B)
    || (byte >= 0x2D && byte <= 0x3A)
    || (byte >= 0x3C && byte <= 0x5B)
    || (byte >= 0x5D && byte <= 0x7E)

percentSign :: Word.Word8
percentSign = 0x25

{- |
  An implementation of ParameterSchema for decoding cookies
-}
newtype CookieDecoder a b
  = CookieDecoder (CookieMap -> Either T.Text b)

decodeCookies :: CookieDecoder a b -> BS.ByteString -> Either T.Text b
decodeCookies (CookieDecoder parseMap) =
  let
    queryMap :: Ord k => [(k, a)] -> Map.Map k (DList.DList a)
    queryMap =
      Map.fromListWith (flip (<>))
        . fmap (fmap DList.singleton)
  in
    parseMap . queryMap . Cookie.parseCookies

type CookieMap = Map.Map BS.ByteString (DList.DList BS.ByteString)

instance PS.ParameterSchema CookieDecoder where
  newtype Parameter CookieDecoder _record a = DecodeCookie (CookieMap -> Either T.Text a)

  makeParams constructor =
    CookieDecoder (\_cookieMap -> Right constructor)

  validateParams _unvalidate validate (CookieDecoder f) =
    CookieDecoder (validate <=< f)

  addParam (CookieDecoder f) (DecodeCookie g) =
    CookieDecoder (\cookieMap -> f cookieMap <*> g cookieMap)

  required _accessor paramDef =
    let
      paramName =
        PD.parameterName paramDef

      paramNameBytes =
        Enc.encodeUtf8 $
          paramName
    in
      DecodeCookie $ \cookieMap -> do
        mbValue <- PL.lookupSingleValue paramName paramNameBytes cookieMap
        case mbValue of
          Just value -> decodeCookieValue paramDef value
          Nothing -> Left $ T.pack "Required cookie param missing: " <> paramName

  optional _accessor paramDef =
    let
      paramName =
        PD.parameterName paramDef

      paramNameBytes =
        Enc.encodeUtf8 $
          paramName
    in
      DecodeCookie $ \cookieMap -> do
        mbValue <- PL.lookupSingleValue paramName paramNameBytes cookieMap
        case mbValue of
          Just value -> Just <$> decodeCookieValue paramDef value
          Nothing -> Right Nothing

  splat _accessor (CookieDecoder f) =
    DecodeCookie f

decodeCookieValue :: PD.ParameterDefinition param -> BS.ByteString -> Either T.Text param
decodeCookieValue paramDef =
  PL.decodeParamBytes paramDef . HTTPTypes.urlDecode False
