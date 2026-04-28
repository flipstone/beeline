{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Copyright : Flipstone Technology Partners 2021-2025
License   : MIT

@since 0.1.0.0
-}
module Beeline.Params.ParameterDefinition
  ( ParameterDefinition (..)
  , ParameterType (..)
  , textParam
  , boundedTextParam
  , nonEmptyTextParam
  , integralParam
  , integerParam
  , intParam
  , int8Param
  , int16Param
  , int32Param
  , int64Param
  , scientificParam
  , doubleParam
  , floatParam
  , booleanParam
  , boundedEnumParam
  , coerceParam
  , convertParam
  , parsedParam
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Bifunctor as Bifunctor
import Data.Bool (bool)
import qualified Data.BoundedText as BT
import Data.Coerce (Coercible, coerce)
import Data.Functor (($>))
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import qualified Data.NonEmptyText as NET
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder.Int as LTBI
import qualified Data.Text.Lazy.Builder.RealFloat as LTBF
import GHC.TypeLits (KnownNat)

data ParameterDefinition param = ParameterDefinition
  { parameterName :: Text
  , parameterType :: ParameterType
  , parameterFormat :: Maybe Text
  , parameterMinLength :: Maybe Word
  , parameterMaxLength :: Maybe Word
  , parameterParser :: Text -> Either Text param
  , parameterRenderer :: param -> Text
  }

{- |
  The primitive parameter types that are currently supported.
  These correspond to the parameter types defined by open api,
  except that null, object and array are omitted. Array's are
  supported in Query and Header params via parameter collection
  schemas. Objects, enums and nulls are not supported at this time.
-}
data ParameterType
  = ParameterString
  | ParameterNumber
  | ParameterInteger
  | ParameterBoolean

textParam :: Text -> ParameterDefinition Text
textParam name =
  ParameterDefinition
    { parameterName = name
    , parameterType = ParameterString
    , parameterFormat = Nothing
    , parameterMinLength = Nothing
    , parameterMaxLength = Nothing
    , parameterParser = Right
    , parameterRenderer = id
    }

integerParam :: Text -> ParameterDefinition Integer
integerParam =
  integralParam ParameterInteger Nothing

intParam :: Text -> ParameterDefinition Int
intParam =
  integralParam ParameterInteger Nothing

int8Param :: Text -> ParameterDefinition Int8
int8Param =
  integralParam ParameterInteger (Just (T.pack "int8"))

int16Param :: Text -> ParameterDefinition Int16
int16Param =
  integralParam ParameterInteger (Just (T.pack "int16"))

int32Param :: Text -> ParameterDefinition Int32
int32Param =
  integralParam ParameterInteger (Just (T.pack "int32"))

int64Param :: Text -> ParameterDefinition Int64
int64Param =
  integralParam ParameterInteger (Just (T.pack "int64"))

scientificParam :: Text -> ParameterDefinition Scientific
scientificParam =
  parsedParam
    ParameterNumber
    Nothing
    Atto.scientific
    (T.pack . show)

doubleParam :: Text -> ParameterDefinition Double
doubleParam =
  parsedParam
    ParameterNumber
    (Just (T.pack "double"))
    Atto.double
    (LT.toStrict . LTB.toLazyText . LTBF.realFloat)

floatParam :: Text -> ParameterDefinition Float
floatParam =
  convertParam
    (Just (T.pack "float"))
    (Right . toRealFloat)
    fromFloatDigits
    . scientificParam

booleanParam :: Text -> ParameterDefinition Bool
booleanParam =
  let
    trueText = T.pack "true"
    falseText = T.pack "false"
    parseTrue = Atto.asciiCI trueText $> True
    parseFalse = Atto.asciiCI falseText $> False
    parseBool = parseTrue <|> parseFalse

    renderBool =
      bool trueText falseText
  in
    parsedParam ParameterBoolean Nothing parseBool renderBool

boundedEnumParam ::
  (Enum a, Bounded a) =>
  (a -> T.Text) ->
  Text ->
  ParameterDefinition a
boundedEnumParam toText =
  let
    parsingMap =
      Map.fromList
        . fmap (\enum -> (toText enum, enum))
        $ [minBound .. maxBound]

    parseEnum text =
      case Map.lookup text parsingMap of
        Just enum -> Right enum
        Nothing -> Left (T.pack "Invalid enum value: " <> text)
  in
    convertParam Nothing parseEnum toText . textParam

integralParam ::
  Integral n =>
  ParameterType ->
  Maybe Text ->
  Text ->
  ParameterDefinition n
integralParam paramType format =
  parsedParam
    paramType
    format
    (Atto.signed Atto.decimal)
    (LT.toStrict . LTB.toLazyText . LTBI.decimal)

coerceParam :: Coercible a b => ParameterDefinition b -> ParameterDefinition a
coerceParam =
  convertParam Nothing (Right . coerce) coerce

parsedParam ::
  ParameterType ->
  Maybe Text ->
  Atto.Parser param ->
  (param -> Text) ->
  Text ->
  ParameterDefinition param
parsedParam paramType format attoParser renderer name =
  let
    parser =
      Bifunctor.first T.pack
        . Atto.parseOnly (attoParser <* Atto.endOfInput)
  in
    ParameterDefinition
      { parameterName = name
      , parameterType = paramType
      , parameterFormat = format
      , parameterMinLength = Nothing
      , parameterMaxLength = Nothing
      , parameterParser = parser
      , parameterRenderer = renderer
      }

convertParam ::
  Maybe Text ->
  (a -> Either Text b) ->
  (b -> a) ->
  ParameterDefinition a ->
  ParameterDefinition b
convertParam format parse render paramDef =
  paramDef
    { parameterFormat = format <|> parameterFormat paramDef
    , parameterParser = parse <=< parameterParser paramDef
    , parameterRenderer = parameterRenderer paramDef . render
    }

-- | @since 0.2.0.0
boundedTextParam ::
  forall minLen maxLen.
  (KnownNat minLen, KnownNat maxLen) =>
  Text ->
  ParameterDefinition (BT.BoundedText minLen maxLen)
boundedTextParam name =
  let
    minVal = BT.boundedTextMinLength @(BT.BoundedText minLen maxLen)
    maxVal = BT.boundedTextMaxLength @(BT.BoundedText minLen maxLen)
  in
    ParameterDefinition
      { parameterName = name
      , parameterType = ParameterString
      , parameterFormat = Nothing
      , parameterMinLength = Just (fromIntegral minVal)
      , parameterMaxLength = Just (fromIntegral maxVal)
      , parameterParser = parseBoundedText
      , parameterRenderer = BT.boundedTextToText
      }

parseBoundedText ::
  forall minLen maxLen.
  (KnownNat minLen, KnownNat maxLen) =>
  Text ->
  Either Text (BT.BoundedText minLen maxLen)
parseBoundedText text =
  case BT.boundedTextFromText text of
    Right bt -> Right bt
    Left err -> Left (T.pack (BT.describeBoundedTextError err))

-- | @since 0.2.0.0
nonEmptyTextParam ::
  Text ->
  ParameterDefinition NET.NonEmptyText
nonEmptyTextParam name =
  ParameterDefinition
    { parameterName = name
    , parameterType = ParameterString
    , parameterFormat = Nothing
    , parameterMinLength = Just 1
    , parameterMaxLength = Nothing
    , parameterParser = parseNonEmptyText
    , parameterRenderer = NET.toText
    }

parseNonEmptyText ::
  Text ->
  Either Text NET.NonEmptyText
parseNonEmptyText text =
  case NET.fromText text of
    Just net -> Right net
    Nothing -> Left (T.pack "Text must not be empty")
