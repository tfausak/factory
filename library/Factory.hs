{- |
    This is the main entry point for our API. It doesn't have anything to do
    with Servant per se. It's mostly just Warp and WAI.
-}
module Factory where

import qualified Control.Exception as Exception
import qualified Factory.API as API
import qualified Factory.Server as Server
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant

{- |
    Launch the web server. Use Ctrl-C to stop it.
-}
main :: IO ()
main = do
    let port = 8080 :: Int
    putStrLn ("Starting on port " ++ show port ++ "...")
    Exception.catch
        (Warp.run port application)
        (\ Exception.UserInterrupt -> putStrLn "\nStopping...")

{- |
    The Servant API as a WAI application.
-}
application :: Wai.Application
application = Servant.serve API.api Server.server
