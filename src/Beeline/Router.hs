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

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Kind (Type)
import           Data.Typeable (typeOf, showsTypeRep, Typeable)
import           Shrubbery (Union)
import           Shrubbery.TypeList (KnownLength)

import qualified Network.HTTP.Types as HTTP

class Typeable a => HasParam a where
  name :: a -> Text -- for documentation
  name a = T.pack $ (showsTypeRep $ typeOf a) ""

  fromText :: Text -> Either Text a
  toText   :: a -> Text

class Router r where
  data RouteList r (subRoutes :: [Type]) :: *

  piece         :: Text -> r a -> r a
  explicitParam ::
                   (a -> Text) -- the "name" of the parameter for documentation
                -> (Text -> Either Text a)
                -> (a -> Text)
                -> (b -> a)
                -> r (a -> b)
                -> r b

  param :: HasParam a => (b -> a) -> r (a -> b) -> r b
  param = explicitParam name fromText toText

  end         :: HTTP.StdMethod -> a -> r a
  routeList   :: KnownLength types => RouteList r types -> r (Union types)
  addRoute    :: r a -> RouteList r rest -> RouteList r (a : rest)
  emptyRoutes :: RouteList r '[]
