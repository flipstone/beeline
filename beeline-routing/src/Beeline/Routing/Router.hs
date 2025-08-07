{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Copyright : Flipstone Technology Partners 2023-2025
License   : MIT

@since 0.1.0.0
-}
module Beeline.Routing.Router
  ( Router (..)
  , Param (..)
  , Subrouter (..)
  , get
  , post
  , Beeline.Routing.Router.head
  , put
  , delete
  , trace
  , connect
  , options
  , patch
  , (/-)
  , (/+)
  , (/>)
  , (/:)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Network.HTTP.Types as HTTP
import Shrubbery (Union)
import Shrubbery.TypeList (KnownLength)

import Beeline.Params (ParameterDefinition)

data Param route a = Param
  { paramDefinition :: ParameterDefinition a
  , paramAccessor :: route -> a
  }

data Subrouter r route subroute = Subrouter
  { subrouteRouter :: r subroute
  , subrouteAccessor :: route -> subroute
  }

class Router r where
  data RouteList r (subRoutes :: [Type]) :: Type
  data Builder r route a :: Type

  make ::
    a -> Builder r route a

  piece ::
    Builder r route a -> Text -> Builder r route a

  param ::
    Builder r route (a -> b) -> Param route a -> Builder r route b

  method ::
    HTTP.StdMethod -> Builder r route route -> r route

  subrouter ::
    Builder r route (subroute -> route) -> Subrouter r route subroute -> r route

  routeList :: KnownLength types => RouteList r types -> r (Union types)
  addRoute :: r a -> RouteList r rest -> RouteList r (a : rest)
  emptyRoutes :: RouteList r '[]

(/-) ::
  Router r =>
  Builder r route a ->
  Text ->
  Builder r route a
(/-) = piece

(/+) ::
  Router r =>
  Builder r route (a -> b) ->
  Param route a ->
  Builder r route b
(/+) = param

(/>) ::
  Router r =>
  Builder r route (subroute -> route) ->
  Subrouter r route subroute ->
  r route
(/>) = subrouter

infixl 9 />
infixl 9 /-
infixl 9 /+

(/:) :: Router r => r a -> RouteList r rest -> RouteList r (a : rest)
(/:) = addRoute

infixr 9 /:

get :: Router r => Builder r route route -> r route
get = method HTTP.GET

post :: Router r => Builder r route route -> r route
post = method HTTP.POST

head :: Router r => Builder r route route -> r route
head = method HTTP.HEAD

put :: Router r => Builder r route route -> r route
put = method HTTP.PUT

delete :: Router r => Builder r route route -> r route
delete = method HTTP.DELETE

trace :: Router r => Builder r route route -> r route
trace = method HTTP.TRACE

connect :: Router r => Builder r route route -> r route
connect = method HTTP.CONNECT

options :: Router r => Builder r route route -> r route
options = method HTTP.OPTIONS

patch :: Router r => Builder r route route -> r route
patch = method HTTP.PATCH
