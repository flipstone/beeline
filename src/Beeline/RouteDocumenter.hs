{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Beeline.RouteDocumenter
  ( RouteDocumenter(..)
  ) where

import qualified Network.HTTP.Types as HTTP
import           Shrubbery
import           Data.Text (Text)

import           Beeline.Router (Router(..))
import           Beeline.ParameterDefinition (ParameterDefinition(parameterName))

newtype RouteDocumenter a =
  RouteDocumenter
    { documentRoute :: a -> (HTTP.StdMethod, Text)
    }

instance Router RouteDocumenter where
  newtype RouteList RouteDocumenter subRoutes =
    RouteBranches (BranchBuilder subRoutes (HTTP.StdMethod, Text))

  piece         = documentRoutePiece
  param         = documentRouteParam
  end           = documentRouteEnd
  routeList (RouteBranches branches) = RouteDocumenter (dissect (branchBuild branches))
  addRoute (RouteDocumenter route) (RouteBranches branches) = RouteBranches $ branch route branches
  emptyRoutes = RouteBranches branchEnd

documentRoutePiece :: Text -> RouteDocumenter a -> RouteDocumenter a
documentRoutePiece pieceText subDocumenter =
  RouteDocumenter $ \a ->
    let (method, path) = documentRoute subDocumenter a
    in (method, "/" <> pieceText <> path)

documentRouteParam :: ParameterDefinition param
                   -> (route -> param)
                   -> RouteDocumenter (param -> route)
                   -> RouteDocumenter route
documentRouteParam paramDef _ subDocumenter =
  RouteDocumenter $ \a ->
    let (method, path) = documentRoute subDocumenter $ const a
   in (method, "/" <> "{" <> parameterName paramDef <> "}" <> path)

documentRouteEnd :: HTTP.StdMethod -> a -> RouteDocumenter a
documentRouteEnd method _ =
  RouteDocumenter $ const (method, "")
