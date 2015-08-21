{- |
    Haskell functions for consuming the API.
-}
module Factory.Haskell where

import Servant

import qualified Control.Monad.Trans.Either as Either
import qualified Factory.API as API
import qualified Factory.Types.Widget as Widget
import qualified Servant.Client as Servant

{- |
    Run an action. If it's successful, return the value. If it fails, 'error'
    with the message.
-}
run :: Action a -> IO a
run action = do
    result <- Either.runEitherT action
    case result of
        Left message -> error (show message)
        Right x -> return x

{- |
    A convenient type alias for API consumers. This looks the same as
    'Factory.Server.Action', but the 'Servant.ServantErr' comes from
    @Servant.Client@ instead of @Servant@.
-}
type Action a = Either.EitherT Servant.ServantError IO a

{- |
    Get all of the widgets. See 'API.ListWidgets'.
-}
listWidgets :: Action [Widget.Widget]

{- |
    Create a new widget. See 'API.CreateWidget'.
-}
createWidget :: Widget.Widget -> Action Widget.Widget

{- |
    Try to get a particular widget. See 'API.ShowWidget'.
-}
showWidget :: Int -> Action Widget.Widget

{- |
    Update an existing widget. See 'API.UpdateWidget'.
-}
updateWidget :: Int -> Widget.Widget -> Action Widget.Widget

{- |
    Destroy an existing widget. See 'API.DestroyWidget'.
-}
destroyWidget :: Int -> Action Widget.Widget

(   listWidgets
    :<|> createWidget
    :<|> showWidget
    :<|> updateWidget
    :<|> destroyWidget
    ) = Servant.client API.documentedAPI host

{- |
    The default server location.
-}
host :: Servant.BaseUrl
host = Servant.BaseUrl Servant.Http "localhost" 8080
