{-|
  This module provides functions to load and manage the loading of assets.
-}
module FRP.Spice.Internal.Assets ( loadSpriteAsset
                                 , performAssetLoads
                                 ) where

-------------------
-- Local Imports --
import FRP.Spice.Internal.Types

----------
-- Code --

{-|
  Creating a @'LoadAssets'@ from a call to load a sprite from a file.
-}
loadSpriteAsset :: FilePath -> LoadAssets
loadSpriteAsset path = DoListT [LoadSprite path] ()

{-|
  Performing all of the @'LoadAsset'@ commands contained in a @'LoadAssets'@.
-}
performAssetLoads :: LoadAssets -> IO Assets
performAssetLoads _ = undefined
