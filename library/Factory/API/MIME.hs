{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
    Additional MIME types that Servant does not define.
-}
module Factory.API.MIME where

import Network.HTTP.Media ((//), (/:))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Servant

{- |
    The JavaScript MIME type. This is useful for returning the correct MIME
    type for the generated JavaScript client code.
-}
data JavaScript

instance Servant.Accept JavaScript where
    contentType _ = "application" // "javascript" /: ("charset", "utf-8")

{- |
    Convert JavaScript 'Text.Text' into a 'LBS.ByteString' by encoding it as
    UTF-8. This could be a good spot to perform minification or obfuscation.
-}
instance Servant.MimeRender JavaScript Text.Text where
    mimeRender _ text = LBS.fromStrict (Text.encodeUtf8 text)

{- |
    The Markdown MIME type. This is useful for returning the correct MIME type
    for the generated Markdown documentation. That being said, this is kind of
    overkill since Markdown can be served as plain text.
-}
data Markdown

instance Servant.Accept Markdown where
    contentType _ = "text" // "markdown" /: ("charset", "utf-8")

{- |
    Convert Markdown 'Text.Text' into a 'LBS.ByteString' by encoding it as
    UTF-8.
-}
instance Servant.MimeRender Markdown Text.Text where
    mimeRender _ text = LBS.fromStrict (Text.encodeUtf8 text)
