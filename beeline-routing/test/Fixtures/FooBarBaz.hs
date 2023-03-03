{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Fixtures.FooBarBaz
  ( Foo (Foo)
  , Bar (Bar)
  , Baz (Baz)
  , FooBarBaz
  , fooBarBazToText
  , fooBarBazRouter
  , genFooBarBaz
  , handleFooBarBaz
  ) where

import Data.Text (Text)
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Network.HTTP.Types as HTTP
import qualified Shrubbery as Shrubbery

import qualified Beeline.Routing as R

data Foo = Foo deriving (Show)
data Bar = Bar deriving (Show)
data Baz = Baz deriving (Show)

type FooBarBaz = Shrubbery.Union [Foo, Bar, Baz]

fooBarBazToText :: FooBarBaz -> Text
fooBarBazToText =
  handleFooBarBaz (\Foo -> "foo") (\Bar -> "bar") (\Baz -> "baz")

handleFooBarBaz :: (Foo -> a) -> (Bar -> a) -> (Baz -> a) -> FooBarBaz -> a
handleFooBarBaz foo bar baz =
  Shrubbery.dissect $
    Shrubbery.branchBuild $
      Shrubbery.branch foo $
        Shrubbery.branch bar $
          Shrubbery.branch baz $
            Shrubbery.branchEnd

fooBarBazRouter :: R.Router r => r FooBarBaz
fooBarBazRouter =
  R.routeList
    . R.addRoute (R.route Foo $ R.piece "foo" $ R.end HTTP.GET)
    . R.addRoute (R.route Bar $ R.piece "bar" $ R.end HTTP.GET)
    . R.addRoute (R.route Baz $ R.piece "baz" $ R.end HTTP.GET)
    $ R.emptyRoutes

genFooBarBaz :: HH.Gen FooBarBaz
genFooBarBaz =
  Gen.element
    [ Shrubbery.unify Foo
    , Shrubbery.unify Bar
    , Shrubbery.unify Baz
    ]
