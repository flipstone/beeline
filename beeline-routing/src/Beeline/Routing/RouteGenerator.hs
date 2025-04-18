{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright : Flipstone Technology Partners 2021-2025
License   : MIT

@since 0.1.0.0
-}
module Beeline.Routing.RouteGenerator
  ( RouteGenerator (..)
  , generateRoute
  ) where

import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import Shrubbery (BranchBuilder, branch, branchBuild, branchEnd, dissect)

import Beeline.Routing.ParameterDefinition (ParameterDefinition (parameterRenderer))
import qualified Beeline.Routing.Router as Router

type PieceList = DList.DList Text

newtype RouteGenerator route = RouteGenerator
  { generateSubroute :: route -> (HTTP.StdMethod, PieceList)
  }

generateRoute :: RouteGenerator route -> route -> (HTTP.StdMethod, Text)
generateRoute generator route =
  let
    (method, pieces) =
      generateSubroute generator route

    path =
      "/" <> T.intercalate "/" (DList.toList pieces)
  in
    (method, path)

instance Router.Router RouteGenerator where
  newtype RouteList RouteGenerator subRoutes
    = RouteBranches (BranchBuilder subRoutes (HTTP.StdMethod, PieceList))

  newtype Builder RouteGenerator route _a
    = Builder (route -> PieceList)

  make = generateRouteMake
  piece = generateRoutePiece
  param = generateRouteParam
  method = generateBuilderMethod
  subrouter = generateBuilderSubRouter
  routeList (RouteBranches branches) = RouteGenerator (dissect (branchBuild branches))
  addRoute (RouteGenerator route) (RouteBranches branches) = RouteBranches $ branch route branches
  emptyRoutes = RouteBranches branchEnd

generateRouteMake ::
  a ->
  Router.Builder RouteGenerator route a
generateRouteMake _constructor =
  Builder $ const DList.empty

generateRoutePiece ::
  Router.Builder RouteGenerator route a ->
  Text ->
  Router.Builder RouteGenerator route a
generateRoutePiece (Builder generatePrior) pieceText =
  Builder $ \route ->
    DList.snoc (generatePrior route) pieceText

generateRouteParam ::
  Router.Builder RouteGenerator route (a -> b) ->
  Router.Param route a ->
  Router.Builder RouteGenerator route b
generateRouteParam (Builder generatePrior) (Router.Param paramDef accessor) =
  Builder $ \route ->
    let
      param =
        accessor route

      paramText =
        parameterRenderer paramDef param
    in
      DList.snoc (generatePrior route) paramText

generateBuilderMethod ::
  HTTP.StdMethod ->
  Router.Builder RouteGenerator route route ->
  RouteGenerator route
generateBuilderMethod method (Builder buildRoute) =
  RouteGenerator $ \route ->
    let
      pieces = buildRoute route
    in
      (method, pieces)

generateBuilderSubRouter ::
  Router.Builder RouteGenerator route (subroute -> route) ->
  Router.Subrouter RouteGenerator route subrouter ->
  RouteGenerator route
generateBuilderSubRouter (Builder generate) (Router.Subrouter subrouter accessor) =
  RouteGenerator $ \route ->
    let
      pieces =
        generate route

      (method, subroutePieces) =
        generateSubroute
          subrouter
          (accessor route)
    in
      (method, pieces <> subroutePieces)
