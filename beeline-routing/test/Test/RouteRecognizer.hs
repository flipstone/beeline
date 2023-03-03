{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.RouteRecognizer
  ( tests
  ) where

import Data.Text (Text)
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Types as HTTP

import qualified Beeline.Routing as R
import qualified Fixtures.FooBarBaz as FBB
import Fixtures.SimpleNoArgRoute (SimpleNoArgRoute (SimpleNoArgRoute))
import qualified Fixtures.Subrouter as Subrouter
import Fixtures.TextParam (genTextParam, textParamDef)

tests :: IO Bool
tests =
  HH.checkParallel $
    HH.Group
      "RouteRecognizer"
      [ ("piece exactly matches route pieces", prop_piece)
      , ("param extracts a path param", prop_param)
      , ("param extracts multiple path params", prop_multiParam)
      , ("routeList picks the (first) matching route)", prop_routeList)
      , ("subrouter extracts the child route parts", prop_subrouter)
      ]

prop_piece :: HH.Property
prop_piece =
  HH.property $ do
    path <- HH.forAll $ Gen.list (Range.linear 0 10) genPathPiece
    method <- HH.forAll genMethod

    let
      recognizer =
        R.route SimpleNoArgRoute $
          foldr
            R.piece
            (R.end method)
            path

      result =
        R.recognizeRoute recognizer method path

    result === Right SimpleNoArgRoute

prop_param :: HH.Property
prop_param =
  HH.property $ do
    param <- HH.forAll genTextParam
    method <- HH.forAll genMethod

    let
      recognizer =
        R.route id $ R.param textParamDef id $ R.end method

      result =
        R.recognizeRoute recognizer method [R.parameterRenderer textParamDef param]

    result === Right param

prop_multiParam :: HH.Property
prop_multiParam =
  HH.property $ do
    param1 <- HH.forAll genTextParam
    param2 <- HH.forAll genTextParam
    method <- HH.forAll genMethod

    let
      recognizer =
        R.route (,)
          . R.param textParamDef fst
          . R.param textParamDef snd
          $ R.end method

      result =
        R.recognizeRoute
          recognizer
          method
          [ R.parameterRenderer textParamDef param1
          , R.parameterRenderer textParamDef param2
          ]

    result === Right (param1, param2)

prop_routeList :: HH.Property
prop_routeList =
  HH.property $ do
    route <- HH.forAll FBB.genFooBarBaz

    let
      result =
        R.recognizeRoute
          FBB.fooBarBazRouter
          HTTP.GET
          [FBB.fooBarBazToText route]

    fmap FBB.fooBarBazToText result === Right (FBB.fooBarBazToText route)

prop_subrouter :: HH.Property
prop_subrouter =
  HH.property $ do
    route <- HH.forAll Subrouter.genSubroutes

    let
      result =
        R.recognizeRoute
          Subrouter.subrouter
          HTTP.GET
          (Subrouter.subrouteToPieces route)

    fmap Subrouter.subrouteToText result === Right (Subrouter.subrouteToText route)

genPathPiece :: HH.Gen Text
genPathPiece =
  Gen.text (Range.linear 0 10) Gen.unicode

genMethod :: HH.Gen HTTP.StdMethod
genMethod =
  Gen.enumBounded
