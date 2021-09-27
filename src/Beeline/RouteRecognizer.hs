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

import           Beeline.Router

newtype RouteRecognizer a =
  RouteRecognizer
    { recognizeRoute :: HTTP.StdMethod -> [Text] -> Either Text a
    }

instance Router RouteRecognizer where
  newtype RouteList RouteRecognizer subRoutes =
    RouteParser (Parser (Either Text) (HTTP.StdMethod, [Text]) subRoutes)

  piece         = recognizeRoutePiece
  explicitParam = recognizeRouteParam
  end           = recognizeRouteEnd
  routeList     = recognizeRouteRouteList
  addRoute      = recognizeRouteAddRoute
  emptyRoutes   = RouteParser parseEnd

recognizeRoutePiece :: Text -> RouteRecognizer a -> RouteRecognizer a
recognizeRoutePiece expectedPiece subRouter =
  RouteRecognizer $ \method pathPieces ->
    case pathPieces of
      [] -> Left "No path left to match"
      p:ps ->
        if   p == expectedPiece
        then recognizeRoute subRouter method ps
        else Left "No route matched"

recognizeRouteParam :: (a -> Text)
                    -> (Text -> Either Text param)
                    -> (param -> Text)
                    -> (route -> param)
                    -> RouteRecognizer (param -> route)
                    -> RouteRecognizer route
recognizeRouteParam _ parseParam _ _ subParser =
  RouteRecognizer $ \method pathParams ->
    case pathParams of
      [] -> Left "No route matched"
      p:ps -> do
        param <- parseParam p
        endpoint <- recognizeRoute subParser method ps
        pure $ endpoint param

recognizeRouteEnd :: HTTP.StdMethod -> a -> RouteRecognizer a
recognizeRouteEnd expectedMethod a =
  RouteRecognizer $ \method pathPieces ->
    case pathPieces of
      [] ->
        if   expectedMethod == method
        then Right a
        else Left "Different method expected"
      _ -> Left "Path not finished"

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
