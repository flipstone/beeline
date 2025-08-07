{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright : Flipstone Technology Partners 2023-2025
License   : MIT

@since 0.1.3.0
-}
module Beeline.HTTP.Client.ContentType
  ( ContentTypeEncoder (EncodeSchema, toRequestContentType, toRequestBody)
  , ContentTypeDecoder (DecodeSchema, DecodingError, toResponseContentType, parseResponse)
  , ContentTypeDecodingError (ContentTypeDecodingError)
  , PlainText (PlainText)
  , PlainTextEncoding (UTF8, LazyUTF8)
  , OctetStream (OctetStream)
  , OctetStreamEncoding (Bytes, LazyBytes)
  , FormURLEncoded (FormURLEncoded)
  , FormEncoder (FormEncoder)
  ) where

import qualified Control.Exception as Exc
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Kind (Type)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import qualified Network.HTTP.Client as HTTP

import Beeline.Params (QueryEncoder, encodeQueryBare)

class ContentTypeEncoder coder where
  type EncodeSchema coder :: Type -> Type

  toRequestContentType ::
    coder -> EncodeSchema coder a -> BS.ByteString

  toRequestBody ::
    coder -> EncodeSchema coder a -> a -> HTTP.RequestBody

class ContentTypeDecoder coder where
  type DecodeSchema coder :: Type -> Type
  type DecodingError coder :: Type

  toResponseContentType ::
    coder ->
    DecodeSchema coder a ->
    BS.ByteString

  parseResponse ::
    coder ->
    DecodeSchema coder a ->
    HTTP.BodyReader ->
    IO (Either (DecodingError coder) a)

newtype ContentTypeDecodingError
  = ContentTypeDecodingError String
  deriving (Show)

instance Exc.Exception ContentTypeDecodingError

{-
  A content type tag for dealing with plain text
-}
data PlainText
  = PlainText

data PlainTextEncoding a where
  UTF8 :: PlainTextEncoding T.Text
  LazyUTF8 :: PlainTextEncoding LT.Text

instance ContentTypeEncoder PlainText where
  type EncodeSchema PlainText = PlainTextEncoding

  toRequestContentType PlainText =
    plainTextContentType

  toRequestBody PlainText encoding =
    case encoding of
      UTF8 -> HTTP.RequestBodyBS . Enc.encodeUtf8
      LazyUTF8 -> HTTP.RequestBodyLBS . LEnc.encodeUtf8

instance ContentTypeDecoder PlainText where
  type DecodeSchema PlainText = PlainTextEncoding
  type DecodingError PlainText = ContentTypeDecodingError

  toResponseContentType PlainText =
    plainTextContentType

  parseResponse PlainText encoding reader = do
    case encoding of
      LazyUTF8 -> decodeLazyUtf8Body reader
      UTF8 -> fmap (fmap LT.toStrict) (decodeLazyUtf8Body reader)

decodeLazyUtf8Body ::
  HTTP.BodyReader ->
  IO (Either ContentTypeDecodingError LT.Text)
decodeLazyUtf8Body reader = do
  lazyBytes <- readLazyBytes reader
  pure $
    case LEnc.decodeUtf8' lazyBytes of
      Left err -> Left (ContentTypeDecodingError (show err))
      Right text -> Right text

plainTextContentType :: PlainTextEncoding a -> BS.ByteString
plainTextContentType encoding =
  let
    charset =
      case encoding of
        UTF8 -> "utf8"
        LazyUTF8 -> "utf8"
  in
    BS8.pack "text/plain; charset=" <> BS8.pack charset

{-
  A content type tag for dealing with bytes strings
-}
data OctetStream
  = OctetStream

data OctetStreamEncoding a where
  Bytes :: OctetStreamEncoding BS.ByteString
  LazyBytes :: OctetStreamEncoding LBS.ByteString

instance ContentTypeEncoder OctetStream where
  type EncodeSchema OctetStream = OctetStreamEncoding

  toRequestContentType OctetStream _ =
    octetStreamContentType

  toRequestBody OctetStream encoding =
    case encoding of
      Bytes -> HTTP.RequestBodyBS
      LazyBytes -> HTTP.RequestBodyLBS

instance ContentTypeDecoder OctetStream where
  type DecodeSchema OctetStream = OctetStreamEncoding
  type DecodingError OctetStream = ContentTypeDecodingError

  toResponseContentType OctetStream _ =
    octetStreamContentType

  parseResponse OctetStream encoding reader = do
    case encoding of
      Bytes -> fmap (Right . LBS.toStrict) (readLazyBytes reader)
      LazyBytes -> fmap Right (readLazyBytes reader)

octetStreamContentType :: BS.ByteString
octetStreamContentType =
  BS8.pack "application/octet-stream"

readLazyBytes :: HTTP.BodyReader -> IO LBS.ByteString
readLazyBytes =
  fmap LBS.fromChunks . HTTP.brConsume

{-
  A content type tag for posting form encoded bodies
-}

data FormURLEncoded
  = FormURLEncoded

newtype FormEncoder a
  = FormEncoder (QueryEncoder a a)

instance ContentTypeEncoder FormURLEncoded where
  type EncodeSchema FormURLEncoded = FormEncoder

  toRequestContentType FormURLEncoded _ =
    formURLEncodedContentType

  toRequestBody FormURLEncoded (FormEncoder queryEncoder) =
    HTTP.RequestBodyBS . encodeQueryBare queryEncoder

formURLEncodedContentType :: BS.ByteString
formURLEncodedContentType =
  BS8.pack "application/x-www-form-urlencoded"
