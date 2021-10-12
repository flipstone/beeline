{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Beeline.Router
  ( Router
  , RouteList
  , piece
  , param
  , end
  , routeList
  , addRoute
  , emptyRoutes
  ) where

import           Data.Text (Text)
import           Data.Kind (Type)
import           Shrubbery (Union)
import           Shrubbery.TypeList (KnownLength)

import           Beeline.ParameterDefinition (ParameterDefinition)
import qualified Network.HTTP.Types as HTTP

class Router r where
  data RouteList r (subRoutes :: [Type]) :: *

  piece :: Text -> r a -> r a
  param :: ParameterDefinition a
        -> (b -> a)
        -> r (a -> b)
        -> r b

  end         :: HTTP.StdMethod -> a -> r a
  routeList   :: KnownLength types => RouteList r types -> r (Union types)
  addRoute    :: r a -> RouteList r rest -> RouteList r (a : rest)
  emptyRoutes :: RouteList r '[]
