{-# LANGUAGE TypeFamilies #-}

{- |
Copyright : Flipstone Technology Partners 2025
License   : MIT

@since 0.1.0.0
-}
module Beeline.Params.QuerySchema
  ( QuerySchema
      ( explodedArray
      , explodedNonEmpty
      )
  , QueryEncoder (..)
  , encodeQuery
  , encodeQueryBare
  , QueryBuilder
  , QueryDecoder (..)
  , decodeQuery
  , QueryMap
  ) where

import Control.Monad ((<=<))
import qualified Data.ByteString as BS
import qualified Data.DList as DList
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Network.HTTP.Types as HTTPTypes

import qualified Beeline.Params.Internal.ParamLookup as PL
import qualified Beeline.Params.ParameterDefinition as PD
import qualified Beeline.Params.ParameterSchema as PS

class PS.ParameterSchema schema => QuerySchema schema where
  explodedArray ::
    (record -> [param]) ->
    PD.ParameterDefinition param ->
    PS.Parameter schema record [param]

  explodedNonEmpty ::
    (record -> NEL.NonEmpty param) ->
    PD.ParameterDefinition param ->
    PS.Parameter schema record (NEL.NonEmpty param)

type QueryBuilder =
  DList.DList HTTPTypes.QueryItem

{- |
  An implementation of ParameterSchema for encoding queries
-}
newtype QueryEncoder a b
  = QueryEncoder (a -> QueryBuilder)

{- |
  Render a value to a query as a 'BS.ByteString'. The resulting string with
  include a prepended question mark if any values are present to encode.
-}
encodeQuery :: QueryEncoder a b -> a -> BS.ByteString
encodeQuery (QueryEncoder toBuilder) a =
  case DList.toList (toBuilder a) of
    [] -> BS.empty
    nonEmptyList -> HTTPTypes.renderQuery True nonEmptyList

{- |
  Render a query value to a 'BS.ByteString'. The resulting string will
  not include a prepended question mark.
-}
encodeQueryBare :: QueryEncoder a b -> a -> BS.ByteString
encodeQueryBare (QueryEncoder toBuilder) a =
  HTTPTypes.renderQuery False (DList.toList (toBuilder a))

instance PS.ParameterSchema QueryEncoder where
  newtype Parameter QueryEncoder record _a = EncodeParam (record -> QueryBuilder)

  makeParams _constructor =
    QueryEncoder (const mempty)

  validateParams unvalidate _validate (QueryEncoder f) =
    QueryEncoder (f . unvalidate)

  addParam (QueryEncoder f) (EncodeParam g) =
    QueryEncoder $ \a -> f a <> g a

  required accessor paramDef =
    EncodeParam (encodeParam paramDef . accessor)

  optional accessor paramDef =
    EncodeParam $ \record ->
      case accessor record of
        Nothing -> mempty
        Just param -> encodeParam paramDef param

  splat accessor (QueryEncoder f) =
    EncodeParam (f . accessor)

instance QuerySchema QueryEncoder where
  explodedArray accessor paramDef =
    EncodeParam (foldMap (encodeParam paramDef) . accessor)

  explodedNonEmpty accessor paramDef =
    EncodeParam (foldMap (encodeParam paramDef) . accessor)

encodeParam :: PD.ParameterDefinition param -> param -> QueryBuilder
encodeParam paramDef value =
  let
    encodedName =
      Enc.encodeUtf8 (PD.parameterName paramDef)

    encodedValue =
      Enc.encodeUtf8 (PD.parameterRenderer paramDef value)
  in
    DList.singleton (encodedName, Just encodedValue)

{- |
  An implementation of ParameterSchema for decoding queries
-}
newtype QueryDecoder a b
  = QueryDecoder (QueryMap -> Either T.Text b)

decodeQuery :: QueryDecoder a b -> BS.ByteString -> Either T.Text b
decodeQuery (QueryDecoder parseMap) =
  let
    queryMap =
      Map.fromListWith (flip (<>))
        . fmap (fmap (maybe DList.empty DList.singleton))
        . HTTPTypes.parseQuery
  in
    parseMap . queryMap

type QueryMap = Map.Map BS.ByteString (DList.DList BS.ByteString)

instance PS.ParameterSchema QueryDecoder where
  newtype Parameter QueryDecoder _record a = DecodeParam (QueryMap -> Either T.Text a)

  makeParams constructor =
    QueryDecoder (\_query -> Right constructor)

  validateParams _unvalidate validate (QueryDecoder f) =
    QueryDecoder (validate <=< f)

  addParam (QueryDecoder f) (DecodeParam g) =
    QueryDecoder (\query -> f query <*> g query)

  required _accessor paramDef =
    let
      paramName =
        PD.parameterName paramDef

      paramNameBytes =
        Enc.encodeUtf8 paramName
    in
      DecodeParam $ \query -> do
        mbValue <- PL.lookupSingleValue paramName paramNameBytes query
        case mbValue of
          Just value -> PL.decodeParamBytes paramDef value
          Nothing -> Left $ T.pack "Required query param missing: " <> paramName

  optional _accessor paramDef =
    let
      paramName =
        PD.parameterName paramDef

      paramNameBytes =
        Enc.encodeUtf8 paramName
    in
      DecodeParam $ \query -> do
        mbValue <- PL.lookupSingleValue paramName paramNameBytes query
        case mbValue of
          Just value -> Just <$> PL.decodeParamBytes paramDef value
          Nothing -> Right Nothing

  splat _accessor (QueryDecoder f) =
    DecodeParam f

instance QuerySchema QueryDecoder where
  explodedArray _accessor paramDef =
    let
      paramNameBytes =
        Enc.encodeUtf8 (PD.parameterName paramDef)
    in
      DecodeParam (PL.decodeListParamBytes paramDef . Map.lookup paramNameBytes)

  explodedNonEmpty _accessor paramDef =
    let
      paramName =
        PD.parameterName paramDef

      paramNameBytes =
        Enc.encodeUtf8 paramName
    in
      DecodeParam $ \query -> do
        list <-
          PL.decodeListParamBytes paramDef
            . Map.lookup paramNameBytes
            $ query

        case NEL.nonEmpty list of
          Just nonEmptyList -> Right nonEmptyList
          Nothing -> Left $ T.pack "Required query param missing: " <> paramName
