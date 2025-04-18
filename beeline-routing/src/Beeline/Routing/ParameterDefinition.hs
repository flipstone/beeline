{- |
Copyright : Flipstone Technology Partners 2021-2025
License   : MIT

@since 0.1.0.0
-}
module Beeline.Routing.ParameterDefinition
  ( ParameterDefinition (..)
  , textParam
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
import Data.Coerce (Coercible, coerce)
import Data.Functor (($>))
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder.Int as LTBI
import qualified Data.Text.Lazy.Builder.RealFloat as LTBF

data ParameterDefinition param = ParameterDefinition
  { parameterName :: Text
  , parameterParser :: Text -> Either Text param
  , parameterRenderer :: param -> Text
  }

textParam :: Text -> ParameterDefinition Text
textParam name =
  ParameterDefinition
    { parameterName = name
    , parameterParser = Right
    , parameterRenderer = id
    }

integerParam :: Text -> ParameterDefinition Integer
integerParam =
  integralParam

intParam :: Text -> ParameterDefinition Int
intParam =
  integralParam

int8Param :: Text -> ParameterDefinition Int8
int8Param =
  integralParam

int16Param :: Text -> ParameterDefinition Int16
int16Param =
  integralParam

int32Param :: Text -> ParameterDefinition Int32
int32Param =
  integralParam

int64Param :: Text -> ParameterDefinition Int64
int64Param =
  integralParam

scientificParam :: Text -> ParameterDefinition Scientific
scientificParam name =
  parsedParam name Atto.scientific (T.pack . show)

doubleParam :: Text -> ParameterDefinition Double
doubleParam name =
  parsedParam
    name
    Atto.double
    (LT.toStrict . LTB.toLazyText . LTBF.realFloat)

floatParam :: Text -> ParameterDefinition Float
floatParam =
  convertParam (Right . toRealFloat) fromFloatDigits . scientificParam

booleanParam :: Text -> ParameterDefinition Bool
booleanParam name =
  let
    trueText = T.pack "true"
    falseText = T.pack "false"
    parseTrue = Atto.asciiCI trueText $> True
    parseFalse = Atto.asciiCI falseText $> False
    parseBool = parseTrue <|> parseFalse

    renderBool =
      bool trueText falseText
  in
    parsedParam name parseBool renderBool

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
    convertParam parseEnum toText . textParam

integralParam :: Integral n => Text -> ParameterDefinition n
integralParam name =
  parsedParam name (Atto.signed Atto.decimal) (LT.toStrict . LTB.toLazyText . LTBI.decimal)

coerceParam :: Coercible a b => ParameterDefinition b -> ParameterDefinition a
coerceParam =
  convertParam (Right . coerce) coerce

parsedParam ::
  Text ->
  Atto.Parser param ->
  (param -> Text) ->
  ParameterDefinition param
parsedParam name attoParser renderer =
  let
    parser =
      Bifunctor.first T.pack
        . Atto.parseOnly (attoParser <* Atto.endOfInput)
  in
    ParameterDefinition
      { parameterName = name
      , parameterParser = parser
      , parameterRenderer = renderer
      }

convertParam ::
  (a -> Either Text b) ->
  (b -> a) ->
  ParameterDefinition a ->
  ParameterDefinition b
convertParam parse render paramDef =
  paramDef
    { parameterParser = parse <=< parameterParser paramDef
    , parameterRenderer = parameterRenderer paramDef . render
    }
