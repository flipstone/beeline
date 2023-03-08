{-# LANGUAGE TypeFamilies #-}

module Beeline.HTTP.Client.QuerySchema
  ( QuerySchema
      ( QueryParam
      , makeQuery
      , addParam
      , required
      , optional
      , explodedArray
      , explodedNonEmpty
      )
  , (?+)
  , QueryEncoder (..)
  , encodeQuery
  , QueryBuilder
  , QueryDecoder (..)
  , decodeQuery
  , QueryMap
  ) where

import qualified Beeline.Routing as R
import qualified Data.ByteString as BS
import qualified Data.DList as DList
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Network.HTTP.Types as HTTPTypes

class QuerySchema q where
  data QueryParam q :: Type -> Type -> Type

  makeQuery ::
    a ->
    q query a

  addParam ::
    q query (param -> c) ->
    QueryParam q query param ->
    q query c

  required ::
    (query -> param) ->
    R.ParameterDefinition param ->
    QueryParam q query param

  optional ::
    (query -> Maybe param) ->
    R.ParameterDefinition param ->
    QueryParam q query (Maybe param)

  explodedArray ::
    (query -> [param]) ->
    R.ParameterDefinition param ->
    QueryParam q query [param]

  explodedNonEmpty ::
    (query -> NEL.NonEmpty param) ->
    R.ParameterDefinition param ->
    QueryParam q query (NEL.NonEmpty param)

(?+) ::
  QuerySchema q =>
  q query (param -> c) ->
  QueryParam q query param ->
  q query c
(?+) = addParam

infixl 9 ?+

type QueryBuilder =
  DList.DList HTTPTypes.QueryItem

{- |
  An implementation of QuerySchema for encoding queries
-}
newtype QueryEncoder a b
  = QueryEncoder (a -> QueryBuilder)

{- |
  Render a query value to a 'BS.ByteString'. The resulting string with
  include a prepende question mark if any values are present to encode.
-}
encodeQuery :: QueryEncoder a b -> a -> BS.ByteString
encodeQuery (QueryEncoder toBuilder) a =
  case DList.toList (toBuilder a) of
    [] -> BS.empty
    nonEmptyList -> HTTPTypes.renderQuery True nonEmptyList

instance QuerySchema QueryEncoder where
  newtype QueryParam QueryEncoder query _a = EncodeParam (query -> QueryBuilder)

  makeQuery _constructor =
    QueryEncoder (\_a -> mempty)

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
    EncodeParam (foldMap (encodeParam paramDef) . NEL.toList . accessor)

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
  An implementation of QuerySchema for decoding queries
-}
newtype QueryDecoder a b
  = QueryDecoder (QueryMap -> Either T.Text b)

decodeQuery :: QueryDecoder a b -> BS.ByteString -> Either T.Text b
decodeQuery (QueryDecoder parseMap) query =
  let
    queryMap =
      Map.fromListWith (flip (<>))
        . map (fmap (maybe DList.empty DList.singleton))
        . HTTPTypes.parseQuery
        $ query
  in
    parseMap queryMap

type QueryMap = Map.Map BS.ByteString (DList.DList BS.ByteString)

lookupSingleParam :: T.Text -> BS.ByteString -> QueryMap -> Either T.Text (Maybe BS.ByteString)
lookupSingleParam paramName paramNameBytes query =
  case Map.lookup paramNameBytes query of
    Just values ->
      case DList.toList values of
        [value] -> Right (Just value)
        [] -> Right Nothing
        (_first : _) ->
          Left $ T.pack "Multi values given for single-valued param: " <> paramName
    Nothing ->
      Right Nothing

instance QuerySchema QueryDecoder where
  newtype QueryParam QueryDecoder _query a = DecodeParam (QueryMap -> Either T.Text a)

  makeQuery constructor =
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
        mbValue <- lookupSingleParam paramName paramNameBytes query
        case mbValue of
          Just value -> decodeParam paramDef value
          Nothing -> Left $ T.pack "Required query param missing: " <> paramName

  optional _accessor paramDef =
    let
      paramName =
        R.parameterName paramDef

      paramNameBytes =
        Enc.encodeUtf8 paramName
    in
      DecodeParam $ \query -> do
        mbValue <- lookupSingleParam paramName paramNameBytes query
        case mbValue of
          Just value -> Just <$> decodeParam paramDef value
          Nothing -> Right Nothing

  explodedArray _accessor paramDef =
    let
      paramNameBytes =
        Enc.encodeUtf8 (R.parameterName paramDef)
    in
      DecodeParam (decodeListParam paramNameBytes paramDef)

  explodedNonEmpty _accessor paramDef =
    let
      paramName =
        R.parameterName paramDef

      paramNameBytes =
        Enc.encodeUtf8 paramName
    in
      DecodeParam $ \query -> do
        list <- decodeListParam paramNameBytes paramDef query
        case NEL.nonEmpty list of
          Just nonEmptyList -> Right nonEmptyList
          Nothing -> Left $ T.pack "Required query param missing: " <> paramName

decodeParam :: R.ParameterDefinition param -> BS.ByteString -> Either T.Text param
decodeParam paramDef value =
  case Enc.decodeUtf8' value of
    Left err -> Left . T.pack . show $ err
    Right text -> R.parameterParser paramDef text

decodeListParam ::
  BS.ByteString ->
  R.ParameterDefinition param ->
  QueryMap ->
  Either T.Text [param]
decodeListParam paramNameBytes paramDef query =
  case Map.lookup paramNameBytes query of
    Nothing -> Right []
    Just values -> fmap DList.toList (traverse (decodeParam paramDef) values)
