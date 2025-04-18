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
import qualified Shrubbery

import Beeline.Routing ((/-), (/:))
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
  Shrubbery.dissect
    . Shrubbery.branchBuild
    . Shrubbery.branch foo
    . Shrubbery.branch bar
    . Shrubbery.branch baz
    $ Shrubbery.branchEnd

fooBarBazRouter :: R.Router r => r FooBarBaz
fooBarBazRouter =
  R.routeList $
    R.get (R.make Foo /- "foo")
      /: R.get (R.make Bar /- "bar")
      /: R.get (R.make Baz /- "baz")
      /: R.emptyRoutes

genFooBarBaz :: HH.Gen FooBarBaz
genFooBarBaz =
  Gen.element
    [ Shrubbery.unify Foo
    , Shrubbery.unify Bar
    , Shrubbery.unify Baz
    ]
