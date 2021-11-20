{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Beeline.RouteDocumenter
  ( RouteDocumenter(..)
  , documentRoute
  ) where

import qualified Network.HTTP.Types as HTTP
import           Shrubbery
import           Data.Text (Text)
import qualified Data.Text as T

import           Beeline.Router (Router(..))
import           Beeline.ParameterDefinition (ParameterDefinition(parameterName))

newtype RouteDocumenter route =
  RouteDocumenter
    { documentSubroute :: route -> ([Text] -> [Text]) -> (HTTP.StdMethod, Text)
    }

documentRoute :: RouteDocumenter route -> route -> (HTTP.StdMethod, Text)
documentRoute documenter route =
  documentSubroute documenter route id

instance Router RouteDocumenter where
  newtype RouteList RouteDocumenter subRoutes =
    RouteBranches (BranchBuilder subRoutes (([Text] -> [Text]) -> (HTTP.StdMethod, Text)))

  newtype RoutePieces RouteDocumenter route a =
    RoutePieces (route -> ([Text] -> [Text]) -> (HTTP.StdMethod, Text))

  route = documentRouteRoute
  piece = documentRoutePiece
  param = documentRouteParam
  end = documentRouteEnd
  subrouter = documentRouteSubrouter
  routeList (RouteBranches branches) = RouteDocumenter (dissect (branchBuild branches))
  addRoute (RouteDocumenter route) (RouteBranches branches) = RouteBranches $ branch route branches
  emptyRoutes = RouteBranches branchEnd

documentRouteRoute :: a
                   -> RoutePieces RouteDocumenter route (a -> route)
                   -> RouteDocumenter route
documentRouteRoute _ (RoutePieces document) =
  RouteDocumenter document

documentRoutePiece :: Text
                   -> RoutePieces RouteDocumenter route a
                   -> RoutePieces RouteDocumenter route a
documentRoutePiece pieceText (RoutePieces documentRest) =
  RoutePieces $ \route mkPath ->
    documentRest route (mkPath . (pieceText:))

documentRouteParam :: ParameterDefinition param
                   -> (route -> param)
                   -> RoutePieces RouteDocumenter route (constructor -> route)
                   -> RoutePieces RouteDocumenter route ((param -> constructor) -> route)
documentRouteParam paramDef _ (RoutePieces documentRest) =
  RoutePieces $ \route mkPath ->
    let
      paramText =
        "{" <> parameterName paramDef <> "}"
    in
      documentRest route (mkPath . (paramText:))

documentRouteEnd :: HTTP.StdMethod -> RoutePieces RouteDocumenter route (route -> route)
documentRouteEnd method =
  RoutePieces $ \_ mkPath ->
    let
      pathParts =
        mkPath []
    in
      (method, "/" <> T.intercalate "/" pathParts)

documentRouteSubrouter :: (route -> subroute)
                       -> RouteDocumenter subroute
                       -> RoutePieces RouteDocumenter route ((subroute -> route) -> route)
documentRouteSubrouter accessor subdocumentor =
  RoutePieces $ \route mkPath ->
    documentSubroute subdocumentor (accessor route) mkPath
