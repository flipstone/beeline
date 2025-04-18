{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.RouteGenerator
  ( tests
  ) where

#if __GLASGOW_HASKELL__ < 910
import Data.Foldable (foldl')
#endif
import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Types as HTTP

import Beeline.Routing ((/+))
import qualified Beeline.Routing as R
import qualified Fixtures.FooBarBaz as FBB
import Fixtures.SimpleNoArgRoute (SimpleNoArgRoute (SimpleNoArgRoute))
import qualified Fixtures.Subrouter as Subrouter
import Fixtures.TextParam (genTextParam, textParamDef)

tests :: IO Bool
tests =
  HH.checkParallel $
    HH.Group
      "RouteGenerator"
      [ ("piece generates the given text", prop_piece)
      , ("param renders a path parameter", prop_param)
      , ("param renders multiple path parameters", prop_multiParam)
      , ("routeList renders the appropriate route from the list", prop_routeList)
      , ("subrouter renders the route with parent and subrouter parts", prop_subrouter)
      ]

prop_piece :: HH.Property
prop_piece =
  HH.property $ do
    path <- HH.forAll $ Gen.list (Range.linear 0 10) genPathPiece
    method <- HH.forAll genMethod

    let
      generator =
        R.method method $
          foldl'
            R.piece
            (R.make SimpleNoArgRoute)
            path

      result =
        R.generateRoute generator SimpleNoArgRoute

      expectedText =
        "/" <> T.intercalate "/" path

    result === (method, expectedText)

prop_param :: HH.Property
prop_param =
  HH.property $ do
    param <- HH.forAll genTextParam
    method <- HH.forAll genMethod

    let
      generator =
        R.method method $
          R.make id /+ R.Param textParamDef id

      result =
        R.generateRoute generator param

    result === (method, "/" <> R.parameterRenderer textParamDef param)

prop_multiParam :: HH.Property
prop_multiParam =
  HH.property $ do
    param1 <- HH.forAll genTextParam
    param2 <- HH.forAll genTextParam
    method <- HH.forAll genMethod

    let
      generator =
        R.method method $
          R.make (,)
            /+ R.Param textParamDef fst
            /+ R.Param textParamDef snd

      result =
        R.generateRoute generator (param1, param2)

      expectedPath =
        "/"
          <> R.parameterRenderer textParamDef param1
          <> "/"
          <> R.parameterRenderer textParamDef param2

    result === (method, expectedPath)

prop_routeList :: HH.Property
prop_routeList =
  HH.property $ do
    route <- HH.forAll FBB.genFooBarBaz

    let
      result =
        R.generateRoute FBB.fooBarBazRouter route

    result === (HTTP.GET, "/" <> FBB.fooBarBazToText route)

prop_subrouter :: HH.Property
prop_subrouter =
  HH.property $ do
    route <- HH.forAll Subrouter.genSubroutes

    let
      result =
        R.generateRoute Subrouter.subrouter route

    result === (HTTP.GET, "/" <> Subrouter.subrouteToText route)

genPathPiece :: HH.Gen Text
genPathPiece =
  Gen.text (Range.linear 0 10) Gen.unicode

genMethod :: HH.Gen HTTP.StdMethod
genMethod =
  Gen.enumBounded
