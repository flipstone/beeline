module Main
  ( main
  ) where

import qualified Hedgehog.Main as HHM

import qualified Test.CookieSchema as CookieSchema
import qualified Test.HeaderSchema as HeaderSchema
import qualified Test.ParameterDefinition as ParameterDefinition
import qualified Test.QuerySchema as QuerySchema

main :: IO ()
main =
  HHM.defaultMain
    [ ParameterDefinition.tests
    , QuerySchema.tests
    , CookieSchema.tests
    , HeaderSchema.tests
    ]
