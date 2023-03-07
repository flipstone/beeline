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
  , coerceParam
  , convertParam
  , parsedParam
  ) where

import Control.Monad ((<=<))
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Bifunctor as Bifunctor
import Data.Coerce (Coercible, coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder.Int as LTBI

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

integralParam :: Integral n => Text -> ParameterDefinition n
integralParam name =
  parsedParam
    name
    (Atto.signed Atto.decimal)
    (LT.toStrict . LTB.toLazyText . LTBI.decimal)

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
