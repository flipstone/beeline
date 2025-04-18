{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.RouteDocumenter
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
import Fixtures.TextParam (textParamDef)

tests :: IO Bool
tests =
  HH.checkParallel $
    HH.Group
      "RouteDocumenter"
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
        R.documentRoutes generator

      expectedText =
        "/" <> T.intercalate "/" path

    result === [(method, expectedText)]

prop_param :: HH.Property
prop_param =
  HH.property $ do
    method <- HH.forAll genMethod

    let
      generator =
        R.method method $
          R.make id /+ R.Param textParamDef id

      result =
        R.documentRoutes generator

    result === [(method, "/{" <> R.parameterName textParamDef <> "}")]

prop_multiParam :: HH.Property
prop_multiParam =
  HH.property $ do
    method <- HH.forAll genMethod

    let
      generator =
        R.method method $
          R.make (,)
            /+ R.Param textParamDef fst
            /+ R.Param textParamDef snd

      result =
        R.documentRoutes generator

      expectedPath =
        "/{"
          <> R.parameterName textParamDef
          <> "}/{"
          <> R.parameterName textParamDef
          <> "}"

    result === [(method, expectedPath)]

prop_routeList :: HH.Property
prop_routeList =
  HH.withTests 1 . HH.property $ do
    let
      result =
        R.documentRoutes FBB.fooBarBazRouter

    result
      === [ (HTTP.GET, "/foo")
          , (HTTP.GET, "/bar")
          , (HTTP.GET, "/baz")
          ]

prop_subrouter :: HH.Property
prop_subrouter =
  HH.withTests 1 . HH.property $ do
    let
      result =
        R.documentRoutes Subrouter.subrouter

    result
      === [ (HTTP.GET, "/left/foo")
          , (HTTP.GET, "/left/bar")
          , (HTTP.GET, "/left/baz")
          , (HTTP.GET, "/right/foo")
          , (HTTP.GET, "/right/bar")
          , (HTTP.GET, "/right/baz")
          ]

genPathPiece :: HH.Gen Text
genPathPiece =
  Gen.text (Range.linear 0 10) Gen.unicode

genMethod :: HH.Gen HTTP.StdMethod
genMethod =
  Gen.enumBounded
