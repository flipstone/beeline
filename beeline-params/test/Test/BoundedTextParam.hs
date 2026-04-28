{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.BoundedTextParam
  ( tests
  ) where

import qualified Data.BoundedText as BT
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Beeline.Params as P

tests :: IO Bool
tests =
  HH.checkParallel $
    HH.Group
      "BoundedTextParam"
      [ ("prop_boundedTextParamRoundTrip", prop_boundedTextParamRoundTrip)
      , ("prop_boundedTextParamMinLength", prop_boundedTextParamMinLength)
      , ("prop_boundedTextParamMaxLength", prop_boundedTextParamMaxLength)
      , ("prop_boundedTextParamRejectsTooShort", prop_boundedTextParamRejectsTooShort)
      , ("prop_boundedTextParamRejectsTooLong", prop_boundedTextParamRejectsTooLong)
      ]

prop_boundedTextParamRoundTrip :: HH.Property
prop_boundedTextParamRoundTrip =
  let
    paramDef = P.boundedTextParam @3 @10 "foo"
  in
    HH.property $ do
      text <- HH.forAll (Gen.text (Range.linear 3 10) Gen.unicodeAll)
      case BT.boundedTextFromText @3 @10 text of
        Left _ -> pure ()
        Right bt ->
          HH.tripping bt (P.parameterRenderer paramDef) (P.parameterParser paramDef)

prop_boundedTextParamMinLength :: HH.Property
prop_boundedTextParamMinLength =
  HH.withTests 1 . HH.property $ do
    let
      paramDef = P.boundedTextParam @3 @10 "foo"
    P.parameterMinLength paramDef HH.=== Just 3

prop_boundedTextParamMaxLength :: HH.Property
prop_boundedTextParamMaxLength =
  HH.withTests 1 . HH.property $ do
    let
      paramDef = P.boundedTextParam @3 @10 "foo"
    P.parameterMaxLength paramDef HH.=== Just 10

prop_boundedTextParamRejectsTooShort :: HH.Property
prop_boundedTextParamRejectsTooShort =
  HH.property $ do
    text <- HH.forAll (Gen.text (Range.linear 0 2) Gen.unicodeAll)
    case P.parameterParser (P.boundedTextParam @3 @10 "foo") text of
      Left _ -> HH.success
      Right _ -> HH.failure

prop_boundedTextParamRejectsTooLong :: HH.Property
prop_boundedTextParamRejectsTooLong =
  HH.property $ do
    text <- HH.forAll (Gen.text (Range.linear 11 20) Gen.unicodeAll)
    case P.parameterParser (P.boundedTextParam @3 @10 "foo") text of
      Left _ -> HH.success
      Right _ -> HH.failure
