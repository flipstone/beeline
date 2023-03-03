{-# LANGUAGE OverloadedStrings #-}

module Fixtures.TextParam
  ( TextParam
  , textParamDef
  , genTextParam
  ) where

import Data.Text (Text)
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Beeline.Routing as R

newtype TextParam
  = TextParam Text
  deriving (Eq, Show)

textParamDef :: R.ParameterDefinition TextParam
textParamDef =
  R.coerceParam (R.textParam "TextParam")

genTextParam :: HH.Gen TextParam
genTextParam =
  TextParam <$> Gen.text (Range.linear 0 10) Gen.unicode
