{-# LANGUAGE OverloadedStrings, DataKinds #-}
module Fixtures.Subrouter
  ( LeftSubroute(..)
  , RightSubroute(..)
  , subrouter
  , subrouteToText
  , subrouteToPieces
  , genRouter
  , exampleRouteLeftFoo
  ) where

import qualified Data.Text as T
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen

import qualified Beeline as Beeline
import qualified Shrubbery

import qualified Fixtures.FooBarBaz as FooBarBaz

newtype LeftSubroute =
  LeftSubroute
    { unLeftSubroute :: FooBarBaz.FooBarBaz
    } deriving Show

newtype RightSubroute =
  RightSubroute
    { unRightSubroute :: FooBarBaz.FooBarBaz
    } deriving Show

type Subroutes = Shrubbery.Union '[LeftSubroute, RightSubroute]

leftSubroutePath :: T.Text
leftSubroutePath =
  "left"

rightSubroutePath :: T.Text
rightSubroutePath =
  "right"

subrouter :: Beeline.Router r => r Subroutes
subrouter =
  Beeline.routeList
  $ Beeline.addRoute
      ( Beeline.route LeftSubroute
      $ Beeline.piece leftSubroutePath
      $ Beeline.subrouter unLeftSubroute FooBarBaz.fooBarBazRouter
      )
  $ Beeline.addRoute
      ( Beeline.route RightSubroute
      $ Beeline.piece rightSubroutePath
      $ Beeline.subrouter unRightSubroute FooBarBaz.fooBarBazRouter
      )
      Beeline.emptyRoutes

subrouteToText :: Subroutes -> T.Text
subrouteToText =
  T.intercalate "/" . subrouteToPieces

subrouteToPieces :: Subroutes -> [T.Text]
subrouteToPieces =
  Shrubbery.dissect
  $ Shrubbery.branchBuild
  $ Shrubbery.branch (\(LeftSubroute r)  -> [leftSubroutePath, FooBarBaz.fooBarBazToText r])
  $ Shrubbery.branch (\(RightSubroute r) -> [rightSubroutePath, FooBarBaz.fooBarBazToText r])
  $ Shrubbery.branchEnd

genRouter :: HH.Gen Subroutes
genRouter = do
  fun <- Gen.element [Shrubbery.unify . LeftSubroute, Shrubbery.unify . RightSubroute]
  fun <$> FooBarBaz.genFooBarBaz

exampleRouteLeftFoo :: Subroutes
exampleRouteLeftFoo =
    Shrubbery.unify
  $ LeftSubroute
  $ Shrubbery.unify FooBarBaz.Foo
