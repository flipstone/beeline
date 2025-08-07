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
import qualified Data.DList as DList
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
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
  Cookie.renderCookiesBS . DList.toList . f

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
      Enc.encodeUtf8 (PD.parameterRenderer paramDef value)
  in
    DList.singleton (encodedName, encodedValue)

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
          Just value -> PL.decodeParamBytes paramDef value
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
          Just value -> Just <$> PL.decodeParamBytes paramDef value
          Nothing -> Right Nothing

  splat _accessor (CookieDecoder f) =
    DecodeCookie f
