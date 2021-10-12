{-# LANGUAGE OverloadedStrings #-}
module Fixtures.TextParam
  ( TextParam
  , textParamDef
  , genTextParam
  ) where

import           Data.Text (Text)
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Beeline as Beeline

newtype TextParam =
  TextParam Text
  deriving (Eq, Show)

textParamDef :: Beeline.ParameterDefinition TextParam
textParamDef =
  Beeline.coerceParam (Beeline.textParam "TextParam")

genTextParam :: HH.Gen TextParam
genTextParam =
  TextParam <$> Gen.text (Range.linear 0 10) Gen.unicode
