{-# LANGUAGE OverloadedStrings #-}
module Fixtures.TextParam
  ( TextParam
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

instance Beeline.HasParam TextParam where
  name _ = "TextParam"
  parseParam = Right . TextParam
  renderParam (TextParam txt) = txt

genTextParam :: HH.Gen TextParam
genTextParam =
  TextParam <$> Gen.text (Range.linear 0 10) Gen.unicode
