{-# LANGUAGE OverloadedStrings #-}

module Test.NonEmptyTextParam
  ( tests
  ) where

import qualified Data.NonEmptyText as NET
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Beeline.Params as P

tests :: IO Bool
tests =
  HH.checkParallel $
    HH.Group
      "NonEmptyTextParam"
      [ ("prop_nonEmptyTextParamRoundTrip", prop_nonEmptyTextParamRoundTrip)
      , ("prop_nonEmptyTextParamMinLength", prop_nonEmptyTextParamMinLength)
      , ("prop_nonEmptyTextParamMaxLength", prop_nonEmptyTextParamMaxLength)
      , ("prop_nonEmptyTextParamRejectsEmpty", prop_nonEmptyTextParamRejectsEmpty)
      ]

prop_nonEmptyTextParamRoundTrip :: HH.Property
prop_nonEmptyTextParamRoundTrip =
  let
    paramDef = P.nonEmptyTextParam "foo"
  in
    HH.property $ do
      text <- HH.forAll (Gen.text (Range.linear 1 100) Gen.unicodeAll)
      case NET.fromText text of
        Nothing -> pure ()
        Just net ->
          HH.tripping net (P.parameterRenderer paramDef) (P.parameterParser paramDef)

prop_nonEmptyTextParamMinLength :: HH.Property
prop_nonEmptyTextParamMinLength =
  HH.withTests 1 . HH.property $ do
    let
      paramDef = P.nonEmptyTextParam "foo"
    P.parameterMinLength paramDef HH.=== Just 1

prop_nonEmptyTextParamMaxLength :: HH.Property
prop_nonEmptyTextParamMaxLength =
  HH.withTests 1 . HH.property $ do
    let
      paramDef = P.nonEmptyTextParam "foo"
    P.parameterMaxLength paramDef HH.=== Nothing

prop_nonEmptyTextParamRejectsEmpty :: HH.Property
prop_nonEmptyTextParamRejectsEmpty =
  HH.withTests 1 . HH.property $ do
    case P.parameterParser (P.nonEmptyTextParam "foo") "" of
      Left _ -> HH.success
      Right _ -> HH.failure
