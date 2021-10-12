module Beeline.ParameterDefinition
  ( ParameterDefinition(..)
  , textParam
  , integralParam
  , coerceParam
  , convertParam
  , parsedParam
  ) where

import           Control.Monad ((<=<))
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Bifunctor as Bifunctor
import           Data.Coerce (Coercible, coerce)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder.Int as LTBI

data ParameterDefinition param =
  ParameterDefinition
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

parsedParam :: Text
            -> Atto.Parser param
            -> (param -> Text)
            -> ParameterDefinition param
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

convertParam :: (a -> Either Text b)
             -> (b -> a)
             -> ParameterDefinition a
             -> ParameterDefinition b
convertParam parse render paramDef =
  paramDef
    { parameterParser = parse <=< parameterParser paramDef
    , parameterRenderer = parameterRenderer paramDef . render
    }
