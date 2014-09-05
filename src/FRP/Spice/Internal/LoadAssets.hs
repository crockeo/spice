{-|
  This module provides functions to cleanly assemble @'LoadAssets'@ objects.
-}
module FRP.Spice.Internal.LoadAssets where

-------------------
-- Local Imports --
import FRP.Spice.Internal.Types

----------
-- Code --

{-|
  Creating a @'LoadAsset'@ call to load a @'Sprite'@.
-}
loadSpriteAsset :: FilePath -> LoadAssets
loadSpriteAsset path = DoListT [LoadSprite path] ()

{-|
  Creating a @'LoadAsset'@ call to load a @'Sound'@.
-}
loadSoundAsset :: FilePath -> LoadAssets
loadSoundAsset path = DoListT [LoadSound path] ()
