{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.RouteGenerator
  ( tests
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
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
    HH.Group "RouteGenerator"
      [ ("piece generates the given text", prop_piece)
      , ("param renders a path parameter", prop_param)
      , ("param renders multiple path parameters", prop_multiParam)
      , ("routeList renders the appropriate route from the list", prop_routeList)
      ]

prop_piece :: HH.Property
prop_piece =
  HH.property $ do
    path <- HH.forAll $ Gen.list (Range.linear 0 10) genPathPiece
    method <- HH.forAll genMethod

    let
      generator =
        Beeline.route SimpleNoArgRoute $
          foldr
            Beeline.piece
            (Beeline.end method)
            path

      result =
        Beeline.generateRoute generator SimpleNoArgRoute

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
        Beeline.route id $ Beeline.param textParamDef id $ Beeline.end method

      result =
        Beeline.generateRoute generator param

    result === (method, "/" <> Beeline.parameterRenderer textParamDef param)

prop_multiParam :: HH.Property
prop_multiParam =
  HH.property $ do
    param1 <- HH.forAll genTextParam
    param2 <- HH.forAll genTextParam
    method <- HH.forAll genMethod

    let
      generator =
        Beeline.route (,)
        $ Beeline.param textParamDef fst
        $ Beeline.param textParamDef snd
        $ Beeline.end method

      result =
        Beeline.generateRoute generator (param1, param2)

      expectedPath =
        "/"
        <> Beeline.parameterRenderer textParamDef param1
        <> "/"
        <> Beeline.parameterRenderer textParamDef param2

    result === (method, expectedPath)

prop_routeList :: HH.Property
prop_routeList =
  HH.property $ do
    route <- HH.forAll FBB.genFooBarBaz

    let
      result =
        Beeline.generateRoute FBB.fooBarBazRouter route

    result === (HTTP.GET, "/" <> FBB.fooBarBazToText route)

genPathPiece :: HH.Gen Text
genPathPiece =
  Gen.text (Range.linear 0 10) Gen.unicode

genMethod :: HH.Gen HTTP.StdMethod
genMethod =
  Gen.enumBounded

