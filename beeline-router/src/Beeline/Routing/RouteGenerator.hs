{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Beeline.Routing.RouteGenerator
  ( RouteGenerator (..)
  , generateRoute
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import Shrubbery (BranchBuilder, branch, branchBuild, branchEnd, dissect)

import Beeline.Routing.ParameterDefinition (ParameterDefinition (parameterRenderer))
import Beeline.Routing.Router (Router (..))

newtype RouteGenerator route = RouteGenerator
  { generateSubroute :: route -> ([Text] -> [Text]) -> (HTTP.StdMethod, Text)
  }

generateRoute :: RouteGenerator route -> route -> (HTTP.StdMethod, Text)
generateRoute generator route =
  generateSubroute generator route id

instance Router RouteGenerator where
  newtype RouteList RouteGenerator subRoutes
    = RouteBranches (BranchBuilder subRoutes (([Text] -> [Text]) -> (HTTP.StdMethod, Text)))

  newtype RoutePieces RouteGenerator route a
    = RoutePieces (route -> ([Text] -> [Text]) -> (HTTP.StdMethod, Text))

  route = generateRouteRoute
  piece = generateRoutePiece
  param = generateRouteParam
  end = generateRoutePiecesEnd
  subrouter = generateRoutePiecesSubRouter
  routeList (RouteBranches branches) = RouteGenerator (dissect (branchBuild branches))
  addRoute (RouteGenerator route) (RouteBranches branches) = RouteBranches $ branch route branches
  emptyRoutes = RouteBranches branchEnd

generateRouteRoute ::
  a ->
  RoutePieces RouteGenerator route (a -> route) ->
  RouteGenerator route
generateRouteRoute _ (RoutePieces f) =
  RouteGenerator f

generateRoutePiece ::
  Text ->
  RoutePieces RouteGenerator route a ->
  RoutePieces RouteGenerator route a
generateRoutePiece pieceText (RoutePieces generateRest) =
  RoutePieces $ \route mkPath ->
    generateRest route (mkPath . (pieceText :))

generateRouteParam ::
  ParameterDefinition a ->
  (route -> a) ->
  RoutePieces RouteGenerator route (c -> route) ->
  RoutePieces RouteGenerator route ((a -> c) -> route)
generateRouteParam paramDef accessor (RoutePieces generateRest) =
  RoutePieces $ \route mkPath ->
    let
      param =
        accessor route

      paramText =
        parameterRenderer paramDef param
    in
      generateRest route (mkPath . (paramText :))

generateRoutePiecesEnd :: HTTP.StdMethod -> RoutePieces RouteGenerator route (route -> route)
generateRoutePiecesEnd method =
  RoutePieces $ \_ mkPath ->
    let
      pathParts =
        mkPath []
    in
      (method, "/" <> T.intercalate "/" pathParts)

generateRoutePiecesSubRouter ::
  (route -> subroute) ->
  RouteGenerator subroute ->
  RoutePieces RouteGenerator route ((subroute -> route) -> route)
generateRoutePiecesSubRouter accessor subrouter =
  RoutePieces $ \route mkPath ->
    generateSubroute subrouter (accessor route) mkPath