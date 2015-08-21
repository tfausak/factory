{- |
    JavaScript code for consuming the API.
-}
module Factory.JavaScript where

import qualified Factory.API as API
import qualified Servant.JQuery as Servant

{- |
    Generate some JavaScript that will use jQuery to talk to the API.
-}
javaScript :: String
javaScript = Servant.jsForAPI API.documentedAPI
