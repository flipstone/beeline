{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright : Flipstone Technology Partners 2021-2025
License   : MIT

@since 0.1.0.0
-}
module Beeline.Routing.RouteRecognizer
  ( RouteRecognizer (..)
  ) where

import Data.Either (rights)
import Data.Text (Text)
import qualified Network.HTTP.Types as HTTP
import Shrubbery (Union)
import Shrubbery.Parser (Parser, parse, parseEnd, parseOption)

import Beeline.Routing.ParameterDefinition (ParameterDefinition (parameterParser))
import qualified Beeline.Routing.Router as Router

newtype RouteRecognizer a = RouteRecognizer
  { recognizeRoute :: HTTP.StdMethod -> [Text] -> Either Text a
  }

instance Router.Router RouteRecognizer where
  newtype RouteList RouteRecognizer subRoutes
    = RouteParser (Parser (Either Text) (HTTP.StdMethod, [Text]) subRoutes)

  data Builder RouteRecognizer _route a
    = Builder ([Text] -> Either Text (a, [Text]))

  make = recognizeRouteMake
  piece = recognizeRoutePiece
  param = recognizeRouteParam
  subrouter = recognizeRouteSubrouter
  method = recognizeRouteMethod
  routeList = recognizeRouteRouteList
  addRoute = recognizeRouteAddRoute
  emptyRoutes = RouteParser parseEnd

recognizeRouteMake ::
  a ->
  Router.Builder RouteRecognizer route a
recognizeRouteMake constructor =
  Builder $ \pieces -> Right (constructor, pieces)

recognizeRoutePiece ::
  Router.Builder RouteRecognizer route a ->
  Text ->
  Router.Builder RouteRecognizer route a
recognizeRoutePiece (Builder recognizePieces) expectedPiece =
  Builder $ \pieces -> do
    (a, remainingPieces) <- recognizePieces pieces
    case remainingPieces of
      [] -> Left "No path left to match"
      p : ps ->
        if p == expectedPiece
          then Right (a, ps)
          else Left "No route matched"

recognizeRouteParam ::
  Router.Builder RouteRecognizer route (a -> b) ->
  Router.Param route a ->
  Router.Builder RouteRecognizer route b
recognizeRouteParam (Builder recognizeF) (Router.Param paramDef _accessor) =
  Builder $ \pieces -> do
    (f, remainingPieces) <- recognizeF pieces
    case remainingPieces of
      [] -> Left "No route matched"
      p : ps -> do
        param <- parameterParser paramDef p
        pure (f param, ps)

recognizeRouteMethod ::
  HTTP.StdMethod ->
  Router.Builder RouteRecognizer route a ->
  RouteRecognizer a
recognizeRouteMethod expectedMethod (Builder recognizePieces) =
  RouteRecognizer $ \method pieces -> do
    (route, remainingPieces) <- recognizePieces pieces
    if null remainingPieces
      then
        if method == expectedMethod
          then Right route
          else Left "Different method expected"
      else Left "Path not finished"

recognizeRouteSubrouter ::
  Router.Builder RouteRecognizer route (subroute -> route) ->
  Router.Subrouter RouteRecognizer route subroute ->
  RouteRecognizer route
recognizeRouteSubrouter (Builder recognizeF) (Router.Subrouter subrouter _accessor) =
  RouteRecognizer $ \method pieces -> do
    (f, remainingPieces) <- recognizeF pieces
    subroute <- recognizeRoute subrouter method remainingPieces
    pure (f subroute)

recognizeRouteAddRoute ::
  RouteRecognizer a ->
  Router.RouteList RouteRecognizer subRoutes ->
  Router.RouteList RouteRecognizer (a : subRoutes)
recognizeRouteAddRoute route (RouteParser parser) =
  let
    runRecognizer (method, pathPieces) = recognizeRoute route method pathPieces
  in
    RouteParser $ parseOption runRecognizer parser

recognizeRouteRouteList ::
  Router.RouteList RouteRecognizer subRoutes ->
  RouteRecognizer (Union subRoutes)
recognizeRouteRouteList (RouteParser parser) =
  RouteRecognizer $ \method pathPieces ->
    let
      results = parse parser (method, pathPieces)
    in
      case rights results of
        (first : _) -> Right first
        [] -> Left "No route matched"
