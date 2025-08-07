{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright : Flipstone Technology Partners 2025
License   : MIT

@since 0.1.0.0
-}
module Beeline.Params.HeaderSchema
  ( HeaderSchema
      ( Cookies
      , cookies
      )
  , HeaderEncoder (..)
  , encodeHeaders
  , HeaderBuilder
  , HeaderDecoder (..)
  , HeaderMap
  , decodeHeaders
  ) where

import Control.Monad ((<=<))
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.DList as DList
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Network.HTTP.Types as HTTPTypes

import qualified Beeline.Params.CookieSchema as CS
import qualified Beeline.Params.Internal.ParamLookup as PL
import qualified Beeline.Params.ParameterDefinition as PD
import qualified Beeline.Params.ParameterSchema as PS

class
  ( PS.ParameterSchema schema
  , PS.ParameterSchema (Cookies schema)
  ) =>
  HeaderSchema schema
  where
  type Cookies schema :: Type -> Type -> Type

  cookies ::
    (record -> cookiesRecord) ->
    Cookies schema cookiesRecord cookiesRecord ->
    PS.Parameter schema record cookiesRecord

{- |
  An implementation of ParameterSchema for encoding queries
-}
newtype HeaderEncoder a b
  = HeaderEncoder (a -> HeaderBuilder)

type HeaderBuilder =
  DList.DList HTTPTypes.Header

{- |
  Render a value to a list of HTTP headers.
-}
encodeHeaders :: HeaderEncoder a b -> a -> [HTTPTypes.Header]
encodeHeaders (HeaderEncoder f) =
  DList.toList . f

instance PS.ParameterSchema HeaderEncoder where
  newtype Parameter HeaderEncoder record _a
    = EncodeHeaders (record -> HeaderBuilder)

  makeParams _constructor =
    HeaderEncoder (const mempty)

  validateParams unvalidate _validate (HeaderEncoder f) =
    HeaderEncoder (f . unvalidate)

  addParam (HeaderEncoder f) (EncodeHeaders g) =
    HeaderEncoder $ \a -> f a <> g a

  required accessor paramDef =
    EncodeHeaders (encodeHeader paramDef . accessor)

  optional accessor paramDef =
    EncodeHeaders $ \record ->
      case accessor record of
        Nothing -> mempty
        Just param -> encodeHeader paramDef param

  splat accessor (HeaderEncoder f) =
    EncodeHeaders (f . accessor)

instance HeaderSchema HeaderEncoder where
  type Cookies HeaderEncoder = CS.CookieEncoder

  cookies accessor cookieEncoder =
    EncodeHeaders $ \record ->
      let
        cookiesBS = CS.encodeCookies cookieEncoder (accessor record)
      in
        if BS.null cookiesBS
          then mempty
          else DList.singleton (HTTPTypes.hCookie, cookiesBS)

encodeHeader :: PD.ParameterDefinition param -> param -> HeaderBuilder
encodeHeader paramDef value =
  let
    encodedName =
      Enc.encodeUtf8 (PD.parameterName paramDef)

    encodedValue =
      Enc.encodeUtf8 (PD.parameterRenderer paramDef value)
  in
    DList.singleton (CI.mk encodedName, encodedValue)

{- |
  An implementation of HeaderSchema for decoding queries
-}
newtype HeaderDecoder a b
  = HeaderDecoder (HeaderMap -> Either T.Text b)

decodeHeaders :: HeaderDecoder a b -> [HTTPTypes.Header] -> Either T.Text b
decodeHeaders (HeaderDecoder parseMap) =
  let
    queryMap :: Ord k => [(k, a)] -> Map.Map k (DList.DList a)
    queryMap =
      Map.fromListWith (flip (<>))
        . fmap (fmap DList.singleton)
  in
    parseMap . queryMap

type HeaderMap = Map.Map (CI.CI BS.ByteString) (DList.DList BS.ByteString)

instance PS.ParameterSchema HeaderDecoder where
  newtype Parameter HeaderDecoder _record a = DecodeHeader (HeaderMap -> Either T.Text a)

  makeParams constructor =
    HeaderDecoder (\_headers -> Right constructor)

  validateParams _unvalidate validate (HeaderDecoder f) =
    HeaderDecoder (validate <=< f)

  addParam (HeaderDecoder f) (DecodeHeader g) =
    HeaderDecoder (\headers -> f headers <*> g headers)

  required _accessor paramDef =
    let
      paramName =
        PD.parameterName paramDef

      paramNameBytes =
        CI.mk
          . Enc.encodeUtf8
          $ paramName
    in
      DecodeHeader $ \headers -> do
        mbValue <- PL.lookupSingleValue paramName paramNameBytes headers
        case mbValue of
          Just value -> PL.decodeParamBytes paramDef value
          Nothing -> Left $ T.pack "Required header param missing: " <> paramName

  optional _accessor paramDef =
    let
      paramName =
        PD.parameterName paramDef

      paramNameBytes =
        CI.mk
          . Enc.encodeUtf8
          $ paramName
    in
      DecodeHeader $ \headers -> do
        mbValue <- PL.lookupSingleValue paramName paramNameBytes headers
        case mbValue of
          Just value -> Just <$> PL.decodeParamBytes paramDef value
          Nothing -> Right Nothing

  splat _accessor (HeaderDecoder f) =
    DecodeHeader f

instance HeaderSchema HeaderDecoder where
  type Cookies HeaderDecoder = CS.CookieDecoder

  cookies _accessor cookieDecoder =
    let
      paramNameBytes =
        HTTPTypes.hCookie

      paramName =
        -- NOTE: decodeUtf8 is unsafe in that it will throw an ugle impure
        -- exception if the bytes are not valid utf8. That should never
        -- happen with the Cookie header name though.
        Enc.decodeUtf8 (CI.original paramNameBytes)
    in
      DecodeHeader $ \headers -> do
        mbValue <- PL.lookupSingleValue paramName paramNameBytes headers
        case mbValue of
          Just value -> CS.decodeCookies cookieDecoder value
          Nothing -> CS.decodeCookies cookieDecoder BS.empty
