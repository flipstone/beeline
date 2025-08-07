{-# LANGUAGE TypeFamilies #-}

{- |
Copyright : Flipstone Technology Partners 2023-2025
License   : MIT

@since 0.1.0.0
-}
module Beeline.Params.ParameterSchema
  ( ParameterSchema
      ( Parameter
      , makeParams
      , validateParams
      , addParam
      , required
      , optional
      , splat
      )
  , (?+)
  ) where

import Data.Kind (Type)
import qualified Data.Text as T

import qualified Beeline.Params.ParameterDefinition as PD

class ParameterSchema schema where
  data Parameter schema :: Type -> Type -> Type

  makeParams ::
    a ->
    schema record a

  validateParams ::
    (b -> a) ->
    (c -> Either T.Text d) ->
    schema a c ->
    schema b d

  addParam ::
    schema record (param -> c) ->
    Parameter schema record param ->
    schema record c

  required ::
    (record -> param) ->
    PD.ParameterDefinition param ->
    Parameter schema record param

  optional ::
    (record -> Maybe param) ->
    PD.ParameterDefinition param ->
    Parameter schema record (Maybe param)

  splat ::
    (record -> params) ->
    schema params params ->
    Parameter schema record params

(?+) ::
  ParameterSchema schema =>
  schema record (param -> c) ->
  Parameter schema record param ->
  schema record c
(?+) = addParam

infixl 9 ?+
