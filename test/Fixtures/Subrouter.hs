{-# LANGUAGE OverloadedStrings #-}
module Fixtures.Subrouter
  ( FooBarBazSubroute(..)
  , router
  , routerWithoutWrapper
  , subroutePath
  , subrouteToText
  , subrouteToPieces
  ) where

import qualified Data.Text as T

import qualified Beeline as Beeline

import qualified Fixtures.FooBarBaz as FooBarBaz

newtype FooBarBazSubroute =
  FooBarBazSubroute
    { subrouteFooBarBaz :: FooBarBaz.FooBarBaz
    } deriving Show

subroutePath :: T.Text
subroutePath =
  "fooBarBaz"

router :: Beeline.Router r => r FooBarBazSubroute
router =
  Beeline.route FooBarBazSubroute $
    Beeline.piece subroutePath $
      Beeline.subrouter subrouteFooBarBaz FooBarBaz.fooBarBazRouter

routerWithoutWrapper :: Beeline.Router r => r FooBarBaz.FooBarBaz
routerWithoutWrapper =
  Beeline.route id $
    Beeline.piece subroutePath $
      Beeline.subrouter id FooBarBaz.fooBarBazRouter

subrouteToText :: FooBarBazSubroute -> T.Text
subrouteToText =
  T.intercalate "/" . subrouteToPieces

subrouteToPieces :: FooBarBazSubroute -> [T.Text]
subrouteToPieces (FooBarBazSubroute fooBarBazRoute) =
  [subroutePath, FooBarBaz.fooBarBazToText fooBarBazRoute]
