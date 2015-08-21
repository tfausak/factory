{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{- |
    This is the high-level description of the API. This is the basis for
    everything else, including the server and documentation.
-}
module Factory.API where

import Servant.API as Servant

import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Factory.API.MIME as MIME
import qualified Factory.Types.Widget as Widget

{- |
    This is the full API, including documentation and client code. It is
    separate from the 'documentedAPI' so that we don't have to generate
    documentation for the documentation itself.
-}
type API
    = GetMarkdown
    :<|> GetJavaScript
    :<|> DocumentedAPI

{- |
    A value-level proxy for the type-level API.
-}
api :: Proxy.Proxy API
api = Proxy.Proxy

{- |
    Get the Markdown documentation.
-}
type GetMarkdown = "markdown"
    :> Servant.Get '[MIME.Markdown] Text.Text

{- |
    Get the JavaScript client code.
-}
type GetJavaScript = "javascript"
    :> Servant.Get '[MIME.JavaScript] Text.Text

{- |
    This is the part of the API we want to document and create client code for.
-}
type DocumentedAPI
    = ListWidgets
    :<|> CreateWidget
    :<|> ShowWidget
    :<|> UpdateWidget
    :<|> DestroyWidget

{- |
    A value-level proxy for the type-level documented API.
-}
documentedAPI :: Proxy.Proxy DocumentedAPI
documentedAPI = Proxy.Proxy

{- |
    Get all of the widgets.
-}
type ListWidgets = "widgets"
    :> Servant.Get '[Servant.JSON] [Widget.Widget]

{- |
    Create a new widget.
-}
type CreateWidget = "widgets"
    :> Servant.ReqBody '[Servant.JSON] Widget.Widget
    :> Servant.Post '[Servant.JSON] Widget.Widget

{- |
    Try to get a particular widget.
-}
type ShowWidget = "widget"
    :> Capture "id" Int
    :> Servant.Get '[Servant.JSON] Widget.Widget

{- |
    Update an existing widget.
-}
type UpdateWidget = "widget"
    :> Capture "id" Int
    :> Servant.ReqBody '[Servant.JSON] Widget.Widget
    :> Servant.Put '[Servant.JSON] Widget.Widget

{- |
    Destroy an existing widget.
-}
type DestroyWidget = "widget"
    :> Capture "id" Int
    :> Servant.Delete '[Servant.JSON] Widget.Widget
