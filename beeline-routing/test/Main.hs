module Main
  ( main
  ) where

import qualified Hedgehog.Main as HHM

import qualified Test.RouteDocumenter as RouteDocumenter
import qualified Test.RouteGenerator as RouteGenerator
import qualified Test.RouteRecognizer as RouteRecognizer

main :: IO ()
main =
  HHM.defaultMain
    [ RouteGenerator.tests
    , RouteRecognizer.tests
    , RouteDocumenter.tests
    ]
