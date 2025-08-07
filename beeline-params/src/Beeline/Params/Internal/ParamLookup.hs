{- |
Copyright : Flipstone Technology Partners 2025
License   : MIT

@since 0.1.0.0
-}
module Beeline.Params.Internal.ParamLookup
  ( decodeParamBytes
  , decodeListParamBytes
  , lookupSingleValue
  ) where

import qualified Data.ByteString as BS
import qualified Data.DList as DList
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc

import qualified Beeline.Params.ParameterDefinition as PD

decodeParamBytes :: PD.ParameterDefinition param -> BS.ByteString -> Either T.Text param
decodeParamBytes paramDef value =
  case Enc.decodeUtf8' value of
    Left err -> Left . T.pack . show $ err
    Right text -> PD.parameterParser paramDef text

decodeListParamBytes ::
  PD.ParameterDefinition param ->
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
lookupSingleValue paramName paramNameKey record =
  case Map.lookup paramNameKey record of
    Just values ->
      case DList.toList values of
        [value] -> Right (Just value)
        [] -> Right Nothing
        (_first : _) ->
          Left $ T.pack "Multiple values given for single-valued param: " <> paramName
    Nothing ->
      Right Nothing
