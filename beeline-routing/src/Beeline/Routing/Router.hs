{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Beeline.Routing.Router
  ( Router (..)
  , (#$)
  , (#/)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Shrubbery (Union)
import Shrubbery.TypeList (KnownLength)

import Beeline.Routing.ParameterDefinition (ParameterDefinition)
import qualified Network.HTTP.Types as HTTP

class Router r where
  data RouteList r (subRoutes :: [Type]) :: Type
  data RoutePieces r route a :: Type

  route ::
    a ->
    RoutePieces r route (a -> route) ->
    r route

  piece ::
    Text ->
    RoutePieces r route a ->
    RoutePieces r route a

  param ::
    ParameterDefinition a ->
    (route -> a) ->
    RoutePieces r route (c -> route) ->
    RoutePieces r route ((a -> c) -> route)

  subrouter ::
    (route -> subroute) ->
    r subroute ->
    RoutePieces r route ((subroute -> route) -> route)

  end :: HTTP.StdMethod -> RoutePieces r route (route -> route)

  routeList :: KnownLength types => RouteList r types -> r (Union types)
  addRoute :: r a -> RouteList r rest -> RouteList r (a : rest)
  emptyRoutes :: RouteList r '[]

(#$) ::
  Router r =>
  HTTP.StdMethod ->
  (RoutePieces r route (route -> route) -> t) ->
  t
(#$) method f = f (end method)

(#/) :: (b -> c) -> (a -> b) -> a -> c
(#/) = (.)
