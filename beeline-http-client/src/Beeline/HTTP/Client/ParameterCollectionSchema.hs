{-# LANGUAGE TypeFamilies #-}

{- |
Copyright : Flipstone Technology Partners 2023-2025
License   : MIT

@since 0.7.0.0
-}
module Beeline.HTTP.Client.ParameterCollectionSchema
  ( ParameterCollectionSchema
      ( ParameterCollectionItem
      , makeParams
      , addParam
      , required
      , optional
      , explodedArray
      , explodedNonEmpty
      )
  , (?+)
  , QueryEncoder (..)
  , encodeQuery
  , encodeQueryBare
  , QueryBuilder
  , QueryDecoder (..)
  , decodeQuery
  , QueryMap
  , HeaderEncoder (..)
  , encodeHeaders
  , HeaderBuilder
  , HeaderDecoder (..)
  , decodeHeaders
  , HeaderMap
  ) where

import qualified Beeline.Routing as R
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.DList as DList
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Network.HTTP.Types as HTTPTypes

class ParameterCollectionSchema q where
  data ParameterCollectionItem q :: Type -> Type -> Type

  makeParams ::
    a ->
    q query a

  addParam ::
    q query (param -> c) ->
    ParameterCollectionItem q query param ->
    q query c

  required ::
    (query -> param) ->
    R.ParameterDefinition param ->
    ParameterCollectionItem q query param

  optional ::
    (query -> Maybe param) ->
    R.ParameterDefinition param ->
    ParameterCollectionItem q query (Maybe param)

  explodedArray ::
    (query -> [param]) ->
    R.ParameterDefinition param ->
    ParameterCollectionItem q query [param]

  explodedNonEmpty ::
    (query -> NEL.NonEmpty param) ->
    R.ParameterDefinition param ->
    ParameterCollectionItem q query (NEL.NonEmpty param)

(?+) ::
  ParameterCollectionSchema q =>
  q query (param -> c) ->
  ParameterCollectionItem q query param ->
  q query c
(?+) = addParam

infixl 9 ?+

type QueryBuilder =
  DList.DList HTTPTypes.QueryItem

{- |
  An implementation of ParameterCollectionSchema for encoding queries
-}
newtype QueryEncoder a b
  = QueryEncoder (a -> QueryBuilder)

{- |
  Render a query value to a 'BS.ByteString'. The resulting string with
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

instance ParameterCollectionSchema QueryEncoder where
  newtype ParameterCollectionItem QueryEncoder query _a = EncodeParam (query -> QueryBuilder)

  makeParams _constructor =
    QueryEncoder (const mempty)

  addParam (QueryEncoder f) (EncodeParam g) =
    QueryEncoder $ \a -> f a <> g a

  required accessor paramDef =
    EncodeParam (encodeParam paramDef . accessor)

  optional accessor paramDef =
    EncodeParam $ \query ->
      case accessor query of
        Nothing -> mempty
        Just param -> encodeParam paramDef param

  explodedArray accessor paramDef =
    EncodeParam (foldMap (encodeParam paramDef) . accessor)

  explodedNonEmpty accessor paramDef =
    EncodeParam (foldMap (encodeParam paramDef) . accessor)

encodeParam :: R.ParameterDefinition param -> param -> QueryBuilder
encodeParam paramDef value =
  let
    encodedName =
      Enc.encodeUtf8 (R.parameterName paramDef)

    encodedValue =
      Enc.encodeUtf8 (R.parameterRenderer paramDef value)
  in
    DList.singleton (encodedName, Just encodedValue)

{- |
  An implementation of ParameterCollectionSchema for decoding queries
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

instance ParameterCollectionSchema QueryDecoder where
  newtype ParameterCollectionItem QueryDecoder _query a = DecodeParam (QueryMap -> Either T.Text a)

  makeParams constructor =
    QueryDecoder (\_query -> Right constructor)

  addParam (QueryDecoder f) (DecodeParam g) =
    QueryDecoder (\query -> f query <*> g query)

  required _accessor paramDef =
    let
      paramName =
        R.parameterName paramDef

      paramNameBytes =
        Enc.encodeUtf8 paramName
    in
      DecodeParam $ \query -> do
        mbValue <- lookupSingleValue paramName paramNameBytes query
        case mbValue of
          Just value -> decodeParamBytes paramDef value
          Nothing -> Left $ T.pack "Required query param missing: " <> paramName

  optional _accessor paramDef =
    let
      paramName =
        R.parameterName paramDef

      paramNameBytes =
        Enc.encodeUtf8 paramName
    in
      DecodeParam $ \query -> do
        mbValue <- lookupSingleValue paramName paramNameBytes query
        case mbValue of
          Just value -> Just <$> decodeParamBytes paramDef value
          Nothing -> Right Nothing

  explodedArray _accessor paramDef =
    let
      paramNameBytes =
        Enc.encodeUtf8 (R.parameterName paramDef)
    in
      DecodeParam (decodeListParamBytes paramDef . Map.lookup paramNameBytes)

  explodedNonEmpty _accessor paramDef =
    let
      paramName =
        R.parameterName paramDef

      paramNameBytes =
        Enc.encodeUtf8 paramName
    in
      DecodeParam $ \query -> do
        list <-
          decodeListParamBytes paramDef
            . Map.lookup paramNameBytes
            $ query

        case NEL.nonEmpty list of
          Just nonEmptyList -> Right nonEmptyList
          Nothing -> Left $ T.pack "Required query param missing: " <> paramName

{- |
  An implementation of ParameterCollectionSchema for encoding queries
-}
newtype HeaderEncoder a b
  = HeaderEncoder (a -> HeaderBuilder)

type HeaderBuilder =
  DList.DList HTTPTypes.Header

{- |
  Render a query value to a list of HTTP headers.
-}
encodeHeaders :: HeaderEncoder a b -> a -> [HTTPTypes.Header]
encodeHeaders (HeaderEncoder f) =
  DList.toList . f

instance ParameterCollectionSchema HeaderEncoder where
  newtype ParameterCollectionItem HeaderEncoder query _a
    = EncodeHeaders (query -> HeaderBuilder)

  makeParams _constructor =
    HeaderEncoder (const mempty)

  addParam (HeaderEncoder f) (EncodeHeaders g) =
    HeaderEncoder $ \a -> f a <> g a

  required accessor paramDef =
    EncodeHeaders (encodeHeader paramDef . accessor)

  optional accessor paramDef =
    EncodeHeaders $ \query ->
      case accessor query of
        Nothing -> mempty
        Just param -> encodeHeader paramDef param

  explodedArray accessor paramDef =
    EncodeHeaders (foldMap (encodeHeader paramDef) . accessor)

  explodedNonEmpty accessor paramDef =
    EncodeHeaders (foldMap (encodeHeader paramDef) . accessor)

encodeHeader :: R.ParameterDefinition param -> param -> HeaderBuilder
encodeHeader paramDef value =
  let
    encodedName =
      Enc.encodeUtf8 (R.parameterName paramDef)

    encodedValue =
      Enc.encodeUtf8 (R.parameterRenderer paramDef value)
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

instance ParameterCollectionSchema HeaderDecoder where
  newtype ParameterCollectionItem HeaderDecoder _query a = DecodeHeader (HeaderMap -> Either T.Text a)

  makeParams constructor =
    HeaderDecoder (\_query -> Right constructor)

  addParam (HeaderDecoder f) (DecodeHeader g) =
    HeaderDecoder (\query -> f query <*> g query)

  required _accessor paramDef =
    let
      paramName =
        R.parameterName paramDef

      paramNameBytes =
        CI.mk
          . Enc.encodeUtf8
          $ paramName
    in
      DecodeHeader $ \query -> do
        mbValue <- lookupSingleValue paramName paramNameBytes query
        case mbValue of
          Just value -> decodeParamBytes paramDef value
          Nothing -> Left $ T.pack "Required header param missing: " <> paramName

  optional _accessor paramDef =
    let
      paramName =
        R.parameterName paramDef

      paramNameBytes =
        CI.mk
          . Enc.encodeUtf8
          $ paramName
    in
      DecodeHeader $ \query -> do
        mbValue <- lookupSingleValue paramName paramNameBytes query
        case mbValue of
          Just value -> Just <$> decodeParamBytes paramDef value
          Nothing -> Right Nothing

  explodedArray _accessor paramDef =
    let
      paramNameBytes =
        CI.mk
          . Enc.encodeUtf8
          . R.parameterName
          $ paramDef
    in
      DecodeHeader (decodeListParamBytes paramDef . Map.lookup paramNameBytes)

  explodedNonEmpty _accessor paramDef =
    let
      paramName =
        R.parameterName paramDef

      paramNameBytes =
        CI.mk
          . Enc.encodeUtf8
          $ paramName
    in
      DecodeHeader $ \headers -> do
        list <-
          decodeListParamBytes paramDef
            . Map.lookup paramNameBytes
            $ headers

        case NEL.nonEmpty list of
          Just nonEmptyList -> Right nonEmptyList
          Nothing -> Left $ T.pack "Required header param missing: " <> paramName

decodeParamBytes :: R.ParameterDefinition param -> BS.ByteString -> Either T.Text param
decodeParamBytes paramDef value =
  case Enc.decodeUtf8' value of
    Left err -> Left . T.pack . show $ err
    Right text -> R.parameterParser paramDef text

decodeListParamBytes ::
  R.ParameterDefinition param ->
  Maybe (DList.DList BS.ByteString) ->
  Either T.Text [param]
decodeListParamBytes paramDef mbValues =
  case mbValues of
    Nothing -> Right []
    Just values -> fmap DList.toList (traverse (decodeParamBytes paramDef) values)

lookupSingleValue ::
  Ord key =>
  T.Text ->
  key ->
  Map.Map key (DList.DList BS.ByteString) ->
  Either T.Text (Maybe BS.ByteString)
lookupSingleValue paramName paramNameKey query =
  case Map.lookup paramNameKey query of
    Just values ->
      case DList.toList values of
        [value] -> Right (Just value)
        [] -> Right Nothing
        (_first : _) ->
          Left $ T.pack "Multiple values given for single-valued param: " <> paramName
    Nothing ->
      Right Nothing
