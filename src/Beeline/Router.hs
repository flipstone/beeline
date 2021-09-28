{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Beeline.Router
  ( Router
  , RouteList
  , HasParam(..)
  , piece
  , explicitParam
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

import qualified Network.HTTP.Types as HTTP

class HasParam a where
  name :: proxy a -> Text -- for documentation
  parseParam :: Text -> Either Text a
  renderParam   :: a -> Text

param :: (HasParam a, Router r) => (b -> a) -> r (a -> b) -> r b
param accessor =
  explicitParam
    (name accessor) -- uses the accessor as a `proxy a` to get the name of the param
    parseParam
    renderParam
    accessor

class Router r where
  data RouteList r (subRoutes :: [Type]) :: *

  piece         :: Text -> r a -> r a
  explicitParam ::  -- |  the name of the parameter for documentation
                   Text
                -> (Text -> Either Text a)
                -> (a -> Text)
                -> (b -> a)
                -> r (a -> b)
                -> r b

  end         :: HTTP.StdMethod -> a -> r a
  routeList   :: KnownLength types => RouteList r types -> r (Union types)
  addRoute    :: r a -> RouteList r rest -> RouteList r (a : rest)
  emptyRoutes :: RouteList r '[]
