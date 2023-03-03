{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Fixtures.Subrouter
  ( LeftSubroute (..)
  , RightSubroute (..)
  , subrouter
  , subrouteToText
  , subrouteToPieces
  , genSubroutes
  , exampleRouteLeftFoo
  ) where

import qualified Data.Text as T
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen

import qualified Beeline.Routing as R
import qualified Shrubbery

import qualified Fixtures.FooBarBaz as FooBarBaz

newtype LeftSubroute = LeftSubroute
  { unLeftSubroute :: FooBarBaz.FooBarBaz
  }
  deriving (Show)

newtype RightSubroute = RightSubroute
  { unRightSubroute :: FooBarBaz.FooBarBaz
  }
  deriving (Show)

type Subroutes = Shrubbery.Union '[LeftSubroute, RightSubroute]

leftSubroutePath :: T.Text
leftSubroutePath =
  "left"

rightSubroutePath :: T.Text
rightSubroutePath =
  "right"

subrouter :: R.Router r => r Subroutes
subrouter =
  R.routeList
    . R.addRoute
      ( R.route LeftSubroute
          . R.piece leftSubroutePath
          $ R.subrouter unLeftSubroute FooBarBaz.fooBarBazRouter
      )
    . R.addRoute
      ( R.route RightSubroute
          . R.piece rightSubroutePath
          $ R.subrouter unRightSubroute FooBarBaz.fooBarBazRouter
      )
    $ R.emptyRoutes

subrouteToText :: Subroutes -> T.Text
subrouteToText =
  T.intercalate "/" . subrouteToPieces

subrouteToPieces :: Subroutes -> [T.Text]
subrouteToPieces =
  Shrubbery.dissect $
    Shrubbery.branchBuild $
      Shrubbery.branch (\(LeftSubroute r) -> [leftSubroutePath, FooBarBaz.fooBarBazToText r]) $
        Shrubbery.branch (\(RightSubroute r) -> [rightSubroutePath, FooBarBaz.fooBarBazToText r]) $
          Shrubbery.branchEnd

genSubroutes :: HH.Gen Subroutes
genSubroutes = do
  fun <- Gen.element [Shrubbery.unify . LeftSubroute, Shrubbery.unify . RightSubroute]
  fun <$> FooBarBaz.genFooBarBaz

exampleRouteLeftFoo :: Subroutes
exampleRouteLeftFoo =
  Shrubbery.unify $
    LeftSubroute $
      Shrubbery.unify FooBarBaz.Foo
