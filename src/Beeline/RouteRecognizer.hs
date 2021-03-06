{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Beeline.RouteRecognizer
  ( RouteRecognizer(..)
  ) where

import qualified Network.HTTP.Types as HTTP
import           Shrubbery (Union)
import           Shrubbery.Parser (Parser, parse, parseEnd, parseOption)
import           Data.Text (Text)
import           Data.Either (rights)

import           Beeline.Router (Router(..))
import           Beeline.ParameterDefinition (ParameterDefinition(parameterParser))

newtype RouteRecognizer a =
  RouteRecognizer
    { recognizeRoute :: HTTP.StdMethod -> [Text] -> Either Text a
    }

instance Router RouteRecognizer where
  newtype RouteList RouteRecognizer subRoutes =
    RouteParser (Parser (Either Text) (HTTP.StdMethod, [Text]) subRoutes)

  data RoutePieces RouteRecognizer route a where
    RoutePieces :: (c -> HTTP.StdMethod -> [Text] -> Either Text route)
                -> RoutePieces RouteRecognizer route (c -> route)

  route = recognizeRouteRoute
  piece = recognizeRoutePiece
  param = recognizeRouteParam
  subrouter = recognizeRouteSubrouter
  end = recognizeRouteEnd
  routeList = recognizeRouteRouteList
  addRoute = recognizeRouteAddRoute
  emptyRoutes = RouteParser parseEnd

recognizeRouteRoute :: a
                    -> RoutePieces RouteRecognizer route (a -> route)
                    -> RouteRecognizer route
recognizeRouteRoute constructor (RoutePieces recognize) =
  RouteRecognizer $ \method pathPieces ->
    recognize constructor method pathPieces

recognizeRoutePiece :: Text
                    -> RoutePieces RouteRecognizer route a
                    -> RoutePieces RouteRecognizer route a
recognizeRoutePiece expectedPiece (RoutePieces recognizeRest) =
  RoutePieces $ \constructor method path ->
    case path of
      [] -> Left "No path left to match"
      p:ps ->
        if p == expectedPiece
        then recognizeRest constructor method ps
        else Left "No route matched"

recognizeRouteParam :: ParameterDefinition a
                    -> (route -> a)
                    -> RoutePieces RouteRecognizer route (c -> route)
                    -> RoutePieces RouteRecognizer route ((a -> c) -> route)
recognizeRouteParam paramDef _ (RoutePieces recognizeRest) =
  RoutePieces $ \constructor method path ->
    case path of
      [] -> Left "No route matched"
      p:ps -> do
        param <- parameterParser paramDef p
        recognizeRest (constructor param) method ps

recognizeRouteEnd :: HTTP.StdMethod
                  -> RoutePieces RouteRecognizer route (route -> route)
recognizeRouteEnd expectedMethod =
  RoutePieces $ \constructor method path ->
    case path of
      [] ->
        if method == expectedMethod
        then Right constructor
        else Left "Different method expected"
      _ -> Left "Path not finished"

recognizeRouteSubrouter :: (route -> subroute)
                        -> RouteRecognizer subroute
                        -> RoutePieces RouteRecognizer route ((subroute -> route) -> route)
recognizeRouteSubrouter _ subrouter =
  RoutePieces $ \constructor method path ->
    constructor <$> recognizeRoute subrouter method path

recognizeRouteAddRoute :: RouteRecognizer a
                       -> RouteList RouteRecognizer subRoutes
                       -> RouteList RouteRecognizer (a : subRoutes)
recognizeRouteAddRoute route (RouteParser parser) =
  let runRecognizer (method, pathPieces) = recognizeRoute route method pathPieces
  in RouteParser $ parseOption runRecognizer parser

recognizeRouteRouteList :: RouteList RouteRecognizer subRoutes
                        -> RouteRecognizer (Union subRoutes)
recognizeRouteRouteList (RouteParser parser) =
  RouteRecognizer $ \method pathPieces ->
    let results = parse parser (method, pathPieces)
    in case rights results of
        (first:_) -> Right first
        []        -> Left "No route matched"
