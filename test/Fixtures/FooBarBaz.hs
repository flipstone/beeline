{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Fixtures.FooBarBaz
  ( Foo
  , Bar
  , Baz
  , fooBarBazToText
  , fooBarBazRouter
  , genFooBarBaz
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

fooBarBazToText :: Shrubbery.Union [Foo, Bar, Baz] -> Text
fooBarBazToText =
  Shrubbery.dissect
  $ Shrubbery.branchBuild
  $ Shrubbery.branch (\Foo -> "foo")
  $ Shrubbery.branch (\Bar -> "bar")
  $ Shrubbery.branch (\Baz -> "baz")
  $ Shrubbery.branchEnd

fooBarBazRouter :: Beeline.Router r => r(Shrubbery.Union [Foo, Bar, Baz])
fooBarBazRouter =
  Beeline.routeList
  $ Beeline.addRoute (Beeline.piece "foo" $ Beeline.end HTTP.GET Foo)
  $ Beeline.addRoute (Beeline.piece "bar" $ Beeline.end HTTP.GET Bar)
  $ Beeline.addRoute (Beeline.piece "baz" $ Beeline.end HTTP.GET Baz)
  $ Beeline.emptyRoutes

genFooBarBaz :: HH.Gen (Shrubbery.Union [Foo, Bar, Baz])
genFooBarBaz =
  Gen.element
    [ Shrubbery.unify Foo
    , Shrubbery.unify Bar
    , Shrubbery.unify Baz
    ]
