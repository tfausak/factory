{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
    Markdown documentation for the API.
-}
module Factory.Markdown where

import qualified Factory.API as API
import qualified Factory.Types.Widget as Widget
import qualified Servant
import qualified Servant.Docs as Servant

{- |
    Generate a Markdown description of the API.
-}
markdown :: String
markdown = Servant.markdown (Servant.docs API.documentedAPI)

instance Servant.ToSample Widget.Widget Widget.Widget where
    toSample _ = Just Widget.Widget { Widget.name = "Apple" }

instance Servant.ToSample [Widget.Widget] [Widget.Widget] where
    toSample _ = Just
        [ Widget.Widget { Widget.name = "Apple" }
        , Widget.Widget { Widget.name = "Banana" }
        ]

instance Servant.ToCapture (Servant.Capture "id" Int) where
    toCapture _ = Servant.DocCapture
        "id"
        "The ID of the thing you want to find."
