{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Beeline.RouteGenerator
  ( RouteGenerator(..)
  ) where

import qualified Network.HTTP.Types as HTTP
import           Shrubbery
import           Data.Text (Text)

import           Beeline.Router

newtype RouteGenerator a =
  RouteGenerator
    { generateRoute :: a -> (HTTP.StdMethod, Text)
    }

instance Router RouteGenerator where
  newtype RouteList RouteGenerator subRoutes =
    RouteBranches (BranchBuilder subRoutes (HTTP.StdMethod, Text))

  piece         = generateRoutePiece
  explicitParam = generateRouteParam
  end           = generateRouteEnd
  routeList (RouteBranches branches) = RouteGenerator (dissect (branchBuild branches))
  addRoute (RouteGenerator route) (RouteBranches branches) = RouteBranches $ branch route branches
  emptyRoutes = RouteBranches branchEnd

generateRoutePiece :: Text -> RouteGenerator a -> RouteGenerator a
generateRoutePiece pieceText subGenerator =
  RouteGenerator $ \a ->
    let (method, path) = generateRoute subGenerator a
    in (method, "/" <> pieceText <> path)

generateRouteParam :: (a -> Text)
                   -> (Text -> Either Text param)
                   -> (param -> Text)
                   -> (route -> param)
                   -> RouteGenerator (param -> route)
                   -> RouteGenerator route
generateRouteParam _ _ paramToText getParam subGenerator =
  RouteGenerator $ \a ->
    let param = getParam a
        paramText = paramToText param
        (method, path) = generateRoute subGenerator $ const a
    in (method, "/" <> paramText <> path)

generateRouteEnd :: HTTP.StdMethod -> a -> RouteGenerator a
generateRouteEnd method _ =
  RouteGenerator $ const (method, "")
