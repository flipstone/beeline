{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.RouteRecognizer
  ( tests
  ) where

import           Data.Text (Text)
import           Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Types as HTTP

import qualified Beeline as Beeline
import qualified Fixtures.FooBarBaz as FBB
import           Fixtures.SimpleNoArgRoute (SimpleNoArgRoute(SimpleNoArgRoute))
import           Fixtures.TextParam (textParamDef, genTextParam)

tests :: IO Bool
tests =
  HH.checkParallel $
    HH.Group "RouteRecognizer"
      [ ("piece exactly matches route pieces", prop_piece)
      , ("param extracts a path param", prop_param)
      , ("param extracts multiple path params", prop_multiParam)
      , ("routeList picks the (first) matching route)", prop_routeList)
      ]

prop_piece :: HH.Property
prop_piece =
  HH.property $ do
    path <- HH.forAll $ Gen.list (Range.linear 0 10) genPathPiece
    method <- HH.forAll genMethod

    let
      recognizer =
        Beeline.route SimpleNoArgRoute $
          foldr
            Beeline.piece
            (Beeline.end method)
            path

      result =
        Beeline.recognizeRoute recognizer method path

    result === Right SimpleNoArgRoute

prop_param :: HH.Property
prop_param =
  HH.property $ do
    param <- HH.forAll genTextParam
    method <- HH.forAll genMethod

    let
      recognizer =
        Beeline.route id $ Beeline.param textParamDef id $ Beeline.end method

      result =
        Beeline.recognizeRoute recognizer method [Beeline.parameterRenderer textParamDef param]

    result === Right param

prop_multiParam :: HH.Property
prop_multiParam =
  HH.property $ do
    param1 <- HH.forAll genTextParam
    param2 <- HH.forAll genTextParam
    method <- HH.forAll genMethod

    let
      recognizer =
        Beeline.route (,)
        $ Beeline.param textParamDef fst
        $ Beeline.param textParamDef snd
        $ Beeline.end method

      result =
        Beeline.recognizeRoute
          recognizer
          method
          [ Beeline.parameterRenderer textParamDef param1
          , Beeline.parameterRenderer textParamDef param2
          ]

    result === Right (param1, param2)

prop_routeList :: HH.Property
prop_routeList =
  HH.property $ do
    route <- HH.forAll FBB.genFooBarBaz

    let
      result =
        Beeline.recognizeRoute
          FBB.fooBarBazRouter
          HTTP.GET
          [FBB.fooBarBazToText route]

    fmap FBB.fooBarBazToText result === Right (FBB.fooBarBazToText route)

genPathPiece :: HH.Gen Text
genPathPiece =
  Gen.text (Range.linear 0 10) Gen.unicode

genMethod :: HH.Gen HTTP.StdMethod
genMethod =
  Gen.enumBounded

