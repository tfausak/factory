{- |
    A dummy database. This is used for simplicity. In a real application, you
    would want to use either a normal relational database, like PostgreSQL, or
    a real ACID data store, like acid-state.
-}
module Factory.Database where

import qualified Control.Concurrent.STM as STM
import qualified Factory.Types.Widget as Widget
import qualified Safe
import qualified System.IO.Unsafe as Unsafe

{- |
    Our database. It doesn't hold much.
-}
data Database = Database
    { widgets :: [Widget.Widget] -- ^ All the widgets we know about.
    } deriving (Eq, Read, Show)

{-|
    The default (empty) database.
-}
defaultDatabase :: Database
defaultDatabase = Database
    { widgets = []
    }

{- |
    A reference to our canonical database. Warning: This is implemented with
    'Unsafe.unsafePerformIO'!
-}
database :: STM.TVar Database
database = Unsafe.unsafePerformIO (STM.newTVarIO defaultDatabase)

-- * Helper functions

{- |
    Get all of the widgets.
-}
getWidgets :: IO [Widget.Widget]
getWidgets = do
    db <- STM.readTVarIO database
    return (widgets db)

{- |
    Try to get a particular widget.
-}
getWidget :: Int -> IO (Maybe Widget.Widget)
getWidget number = do
    let index = number - 1
    fmap (flip Safe.atMay index) getWidgets

{- |
    Create a new widget.
-}
createWidget :: Widget.Widget -> IO ()
createWidget widget = do
    STM.atomically $ do
        STM.modifyTVar database $ \ db -> db
            { widgets = widgets db ++ [widget]
            }

{- |
    Update an existing widget.
-}
updateWidget :: Int -> Widget.Widget -> IO ()
updateWidget number widget = do
    STM.atomically $ do
        STM.modifyTVar database $ \ db -> db
            { widgets = map
                (\ (i, x) -> if number == i then widget else x)
                (zip [1 ..] (widgets db))
            }

{- |
    Destroy an existing widget.
-}
destroyWidget :: Int -> IO ()
destroyWidget number = do
    STM.atomically $ do
        STM.modifyTVar database $ \ db -> db
            { widgets = map snd (filter
                (\ (i, _) -> i /= number)
                (zip [1 ..] (widgets db)))
            }
