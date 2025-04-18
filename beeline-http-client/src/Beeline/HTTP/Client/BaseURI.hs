{- |
Copyright : Flipstone Technology Partners 2023-2025
License   : MIT

@since 0.6.1.0
-}
module Beeline.HTTP.Client.BaseURI
  ( BaseURI (BaseURI, host, port, basePath, secure)
  , defaultBaseURI
  , parseBaseURI
  , renderBaseURI
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Network.URI as URI
import qualified Numeric

data BaseURI = BaseURI
  { host :: BS.ByteString
  , port :: Int
  , basePath :: BS.ByteString
  , secure :: Bool
  }
  deriving (Eq, Show)

defaultBaseURI :: BaseURI
defaultBaseURI =
  BaseURI
    { host = BS8.pack "localhost"
    , port = 80
    , basePath = BS8.pack ""
    , secure = False
    }

parseBaseURI :: String -> Either String BaseURI
parseBaseURI string = do
  uri <-
    case URI.parseAbsoluteURI string of
      Nothing -> Left "Invalid absolute URI"
      Just absUri -> Right absUri

  authority <-
    case URI.uriAuthority uri of
      Nothing -> Left "URI Authority missing"
      Just auth -> Right auth

  https <-
    case URI.uriScheme uri of
      "http:" -> Right False
      "https:" -> Right True
      invalidScheme -> Left ("Invalid URI scheme: " <> invalidScheme)

  uriPort <-
    case URI.uriPort authority of
      (':' : portString) ->
        case Numeric.readDec portString of
          [(portNum, "")] -> Right portNum
          _ -> Left ("Invalid URI port: :" <> portString)
      "" ->
        Right $
          if https
            then 443
            else 80
      invalidPort ->
        Left ("Invalid URI port: " <> invalidPort)

  pure $
    BaseURI
      { port = uriPort
      , basePath = BS8.pack (URI.uriPath uri)
      , host = BS8.pack (URI.uriRegName authority)
      , secure = https
      }

renderBaseURI :: BaseURI -> String
renderBaseURI baseURI =
  let
    protocol =
      if secure baseURI
        then "https://"
        else "http://"

    portString =
      Numeric.showInt
        (port baseURI)
        ""
  in
    protocol
      <> BS8.unpack (host baseURI)
      <> ":"
      <> portString
      <> BS8.unpack (basePath baseURI)
