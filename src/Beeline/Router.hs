{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Beeline.Router
  ( Router(..)
  , RouteList
  , RoutePieces
  ) where

import           Data.Text (Text)
import           Data.Kind (Type)
import           Shrubbery (Union)
import           Shrubbery.TypeList (KnownLength)

import           Beeline.ParameterDefinition (ParameterDefinition)
import qualified Network.HTTP.Types as HTTP

class Router r where
  data RouteList r (subRoutes :: [Type]) :: *
  data RoutePieces r route a :: *

  route :: a
        -> RoutePieces r route (a -> route)
        -> r route

  piece :: Text
        -> RoutePieces r route a
        -> RoutePieces r route a

  param :: ParameterDefinition a
        -> (route -> a)
        -> RoutePieces r route (c -> route)
        -> RoutePieces r route ((a -> c) -> route)

  subrouter :: (route -> subroute)
            -> r subroute
            -> RoutePieces r route ((subroute -> route) -> route)

  end :: HTTP.StdMethod -> RoutePieces r route (route -> route)

  routeList   :: KnownLength types => RouteList r types -> r (Union types)
  addRoute    :: r a -> RouteList r rest -> RouteList r (a : rest)
  emptyRoutes :: RouteList r '[]
