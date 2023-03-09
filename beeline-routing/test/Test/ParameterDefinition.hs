{-# LANGUAGE OverloadedStrings #-}

module Test.ParameterDefinition
  ( tests
  ) where

import Data.Scientific (scientific)
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Beeline.Routing.ParameterDefinition as P

tests :: IO Bool
tests =
  HH.checkParallel $
    HH.Group
      "ParameterDefinition"
      [ ("prop_textParam", prop_textParam)
      , ("prop_intParam", prop_intParam)
      , ("prop_int8Param", prop_int8Param)
      , ("prop_int16Param", prop_int16Param)
      , ("prop_int32Param", prop_int32Param)
      , ("prop_int64Param", prop_int64Param)
      , ("prop_integerParam", prop_integerParam)
      , ("prop_scientificParam", prop_scientificParam)
      , ("prop_doubleParam", prop_doubleParam)
      , ("prop_floatParam", prop_floatParam)
      ]

prop_textParam :: HH.Property
prop_textParam =
  HH.property $
    trippingParam
      (Gen.text (Range.linear 0 32) Gen.unicodeAll)
      (P.textParam "foo")

prop_intParam :: HH.Property
prop_intParam =
  HH.property $
    trippingParam
      (Gen.int Range.linearBounded)
      (P.intParam "foo")

prop_int8Param :: HH.Property
prop_int8Param =
  HH.property $
    trippingParam
      (Gen.int8 Range.linearBounded)
      (P.int8Param "foo")

prop_int16Param :: HH.Property
prop_int16Param =
  HH.property $
    trippingParam
      (Gen.int16 Range.linearBounded)
      (P.int16Param "foo")

prop_int32Param :: HH.Property
prop_int32Param =
  HH.property $
    trippingParam
      (Gen.int32 Range.linearBounded)
      (P.int32Param "foo")

prop_int64Param :: HH.Property
prop_int64Param =
  HH.property $
    trippingParam
      (Gen.int64 Range.linearBounded)
      (P.int64Param "foo")

prop_integerParam :: HH.Property
prop_integerParam =
  let
    bigInt :: Integer
    bigInt = 2 ^ (256 :: Int)
  in
    HH.property $
      trippingParam
        (Gen.integral (Range.linearFrom 0 (-bigInt) bigInt))
        (P.integerParam "foo")

prop_scientificParam :: HH.Property
prop_scientificParam =
  let
    genScientific =
      scientific
        <$> Gen.integral (Range.linearFrom 0 (-10000) 10000)
        <*> Gen.integral (Range.linearFrom 0 minBound maxBound)
  in
    HH.property $
      trippingParam
        genScientific
        (P.scientificParam "foo")

prop_doubleParam :: HH.Property
prop_doubleParam =
  let
    bigDouble = 1.79769313486231570 ^ (308 :: Int)
  in
    HH.property $
      trippingParam
        (Gen.double (Range.linearFracFrom 0 (-bigDouble) bigDouble))
        (P.doubleParam "foo")

prop_floatParam :: HH.Property
prop_floatParam =
  let
    bigFloat = 3.40282347 ^ (38 :: Int)
  in
    HH.property $
      trippingParam
        (Gen.float (Range.linearFracFrom 0 (-bigFloat) bigFloat))
        (P.floatParam "foo")

trippingParam ::
  (Show a, Eq a) =>
  HH.Gen a ->
  P.ParameterDefinition a ->
  HH.PropertyT IO ()
trippingParam generator paramDef = do
  a <- HH.forAll generator
  HH.tripping a (P.parameterRenderer paramDef) (P.parameterParser paramDef)
