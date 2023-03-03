{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Beeline.Routing.RouteDocumenter
  ( RouteDocumenter (..)
  , documentRoute
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import Shrubbery (BranchBuilder, branch, branchBuild, branchEnd, dissect)

import Beeline.Routing.ParameterDefinition (ParameterDefinition (parameterName))
import qualified Beeline.Routing.Router as Router

newtype RouteDocumenter route = RouteDocumenter
  { documentSubroute :: route -> ([Text] -> [Text]) -> (HTTP.StdMethod, Text)
  }

documentRoute :: RouteDocumenter route -> route -> (HTTP.StdMethod, Text)
documentRoute documenter route =
  documentSubroute documenter route id

instance Router.Router RouteDocumenter where
  newtype RouteList RouteDocumenter subRoutes
    = RouteBranches (BranchBuilder subRoutes (([Text] -> [Text]) -> (HTTP.StdMethod, Text)))

  newtype RoutePieces RouteDocumenter route _a
    = RoutePieces (route -> ([Text] -> [Text]) -> (HTTP.StdMethod, Text))

  route = documentRouteRoute
  piece = documentRoutePiece
  param = documentRouteParam
  end = documentRouteEnd
  subrouter = documentRouteSubrouter
  routeList (RouteBranches branches) = RouteDocumenter (dissect (branchBuild branches))
  addRoute (RouteDocumenter route) (RouteBranches branches) = RouteBranches $ branch route branches
  emptyRoutes = RouteBranches branchEnd

documentRouteRoute ::
  a ->
  Router.RoutePieces RouteDocumenter route (a -> route) ->
  RouteDocumenter route
documentRouteRoute _ (RoutePieces document) =
  RouteDocumenter document

documentRoutePiece ::
  Text ->
  Router.RoutePieces RouteDocumenter route a ->
  Router.RoutePieces RouteDocumenter route a
documentRoutePiece pieceText (RoutePieces documentRest) =
  RoutePieces $ \route mkPath ->
    documentRest route (mkPath . (pieceText :))

documentRouteParam ::
  ParameterDefinition param ->
  (route -> param) ->
  Router.RoutePieces RouteDocumenter route (constructor -> route) ->
  Router.RoutePieces RouteDocumenter route ((param -> constructor) -> route)
documentRouteParam paramDef _ (RoutePieces documentRest) =
  RoutePieces $ \route mkPath ->
    let
      paramText =
        "{" <> parameterName paramDef <> "}"
    in
      documentRest route (mkPath . (paramText :))

documentRouteEnd ::
  HTTP.StdMethod ->
  Router.RoutePieces RouteDocumenter route (route -> route)
documentRouteEnd method =
  RoutePieces $ \_ mkPath ->
    let
      pathParts =
        mkPath []
    in
      (method, "/" <> T.intercalate "/" pathParts)

documentRouteSubrouter ::
  (route -> subroute) ->
  RouteDocumenter subroute ->
  Router.RoutePieces RouteDocumenter route ((subroute -> route) -> route)
documentRouteSubrouter accessor subdocumentor =
  RoutePieces $ \route mkPath ->
    documentSubroute subdocumentor (accessor route) mkPath
