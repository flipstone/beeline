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
      , ("param extracts a path param", prop_param)
      , ("routeList picks the (first) matching route)", prop_routeList)
      ]

prop_piece :: HH.Property
prop_piece =
  HH.property $ do
    path <- HH.forAll $ Gen.list (Range.linear 0 10) genPathPiece
    method <- HH.forAll genMethod

    let
      generator =
        foldr
          Beeline.piece
          (Beeline.end method SimpleNoArgRoute)
          path

      result =
        Beeline.generateRoute generator SimpleNoArgRoute

      expectedText =
        case path of
          [] -> ""
          _  -> "/" <> T.intercalate "/" path

    result === (method, expectedText)

prop_param :: HH.Property
prop_param =
  HH.property $ do
    param <- HH.forAll genTextParam
    method <- HH.forAll genMethod

    let
      generator =
        Beeline.param textParamDef id $ Beeline.end method id

      result =
        Beeline.generateRoute generator param

    result === (method, "/" <> Beeline.parameterRenderer textParamDef param)

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

