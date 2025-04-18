{- |
Copyright : Flipstone Technology Partners 2023-2025
License   : MIT

@since 0.1.3.0
-}
module Beeline.HTTP.Client.Body
  ( HTTP.RequestBody (..)
  , HTTP.BodyReader
  , HTTP.brRead
  , HTTP.brReadSome
  , HTTP.brConsume
  ) where

import qualified Network.HTTP.Client as HTTP
