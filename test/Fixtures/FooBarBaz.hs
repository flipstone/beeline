{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Fixtures.FooBarBaz
  ( Foo(Foo)
  , Bar(Bar)
  , Baz(Baz)
  , FooBarBaz
  , fooBarBazToText
  , fooBarBazRouter
  , genFooBarBaz
  , handleFooBarBaz
  ) where

import           Data.Text (Text)
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Network.HTTP.Types as HTTP
import qualified Shrubbery as Shrubbery

import qualified Beeline as Beeline

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
  $ Shrubbery.branchBuild
  $ Shrubbery.branch foo
  $ Shrubbery.branch bar
  $ Shrubbery.branch baz
  $ Shrubbery.branchEnd

fooBarBazRouter :: Beeline.Router r => r FooBarBaz
fooBarBazRouter =
  Beeline.routeList
  $ Beeline.addRoute (Beeline.route Foo $ Beeline.piece "foo" $ Beeline.end HTTP.GET)
  $ Beeline.addRoute (Beeline.route Bar $ Beeline.piece "bar" $ Beeline.end HTTP.GET)
  $ Beeline.addRoute (Beeline.route Baz $ Beeline.piece "baz" $ Beeline.end HTTP.GET)
  $ Beeline.emptyRoutes

genFooBarBaz :: HH.Gen FooBarBaz
genFooBarBaz =
  Gen.element
    [ Shrubbery.unify Foo
    , Shrubbery.unify Bar
    , Shrubbery.unify Baz
    ]
