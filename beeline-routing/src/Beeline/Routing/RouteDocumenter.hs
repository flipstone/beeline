{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Beeline.Routing.RouteDocumenter
  ( RouteDocumenter (..)
  , documentRoutes
  ) where

import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP

import Beeline.Routing.ParameterDefinition (ParameterDefinition (parameterName))
import qualified Beeline.Routing.Router as Router

type PieceList = DList.DList Text

newtype RouteDocumenter route = RouteDocumenter
  { documentSubroutes :: [(HTTP.StdMethod, PieceList)]
  }

documentRoutes :: RouteDocumenter route -> [(HTTP.StdMethod, Text)]
documentRoutes documenter =
  let
    mkPath (method, pieces) =
      (method, "/" <> T.intercalate "/" (DList.toList pieces))
  in
    map mkPath (documentSubroutes documenter)

instance Router.Router RouteDocumenter where
  newtype RouteList RouteDocumenter _subRoutes
    = RouteBranches [(HTTP.StdMethod, PieceList)]

  newtype Builder RouteDocumenter _route _a
    = Builder PieceList

  make = documentRouteMake
  piece = documentRoutePiece
  param = documentRouteParam
  method = documentRouteMethod
  subrouter = documentRouteSubrouter
  routeList (RouteBranches branches) =
    RouteDocumenter branches

  addRoute (RouteDocumenter routes) (RouteBranches branchesRoutes) =
    RouteBranches $ routes <> branchesRoutes
  emptyRoutes = RouteBranches []

documentRouteMake ::
  a ->
  Router.Builder RouteDocumenter route a
documentRouteMake _constructor =
  Builder DList.empty

documentRoutePiece ::
  Router.Builder RouteDocumenter route a ->
  Text ->
  Router.Builder RouteDocumenter route a
documentRoutePiece (Builder pieces) pieceText =
  Builder $
    DList.snoc pieces pieceText

documentRouteParam ::
  Router.Builder RouteDocumenter route (param -> a) ->
  Router.Param route param ->
  Router.Builder RouteDocumenter route a
documentRouteParam (Builder pieces) (Router.Param paramDef _) =
  Builder $
    let
      paramText =
        "{" <> parameterName paramDef <> "}"
    in
      DList.snoc pieces paramText

documentRouteMethod ::
  HTTP.StdMethod ->
  Router.Builder RouteDocumenter route route ->
  RouteDocumenter route
documentRouteMethod method (Builder pieces) =
  RouteDocumenter [(method, pieces)]

documentRouteSubrouter ::
  Router.Builder RouteDocumenter route (subroute -> route) ->
  Router.Subrouter RouteDocumenter route subroute ->
  RouteDocumenter route
documentRouteSubrouter (Builder pieces) (Router.Subrouter subrouter _accessor) =
  RouteDocumenter $
    let
      subroutes =
        documentSubroutes subrouter

      prependPieces (method, rest) =
        (method, pieces <> rest)
    in
      map prependPieces subroutes
