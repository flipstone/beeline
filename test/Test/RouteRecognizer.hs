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
import           Fixtures.TextParam (genTextParam)

tests :: IO Bool
tests =
  HH.checkParallel $
    HH.Group "RouteRecognizer"
      [ ("piece exactly matches route pieces", prop_piece)
      , ("param extracts a path param", prop_param)
      , ("routeList picks the (first) matching route)", prop_routeList)
      ]

prop_piece :: HH.Property
prop_piece =
  HH.property $ do
    path <- HH.forAll $ Gen.list (Range.linear 0 10) genPathPiece
    method <- HH.forAll genMethod

    let
      recognizer =
        foldr
          Beeline.piece
          (Beeline.end method SimpleNoArgRoute)
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
        Beeline.param id $ Beeline.end method id

      result =
        Beeline.recognizeRoute recognizer method [Beeline.renderParam param]

    result === Right param

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

