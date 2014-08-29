{-|
  This module provides a clean way to load assets that will later be used in
  the spice-based program.
-}
module FRP.Spice.Assets where

--------------------
-- Global Imports --
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.Default

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Sprite
import FRP.Spice.Utils.DoList

----------
-- Code --

{-|
  A single call to load an asset.
-}
data LoadAsset = LoadSprite FilePath

{-|
  A list of @'LoadAssets'@ that may be composed in do-notation.
-}
type LoadAssets = DoList [LoadAsset]

{-|
  The data structure that contains the loaded assets.
-}
data Assets = Assets { sprites :: Map.Map FilePath Sprite }

{-|
  The default state for the @'Assets'@ data type. Used as the initial state for
  @'performAssetLoads'@.
-}
defaultAssets :: Assets
defaultAssets =
  Assets { sprites = Map.fromList [] }

{-|
  A synonym for @'defaultAssets'@ to fit within the data-default library.
-}
instance Default Assets where
  def = defaultAssets

{-|
  Appending a @'Sprite'@ to an @'Assets'@.
-}
appendSprite :: Assets -> FilePath -> Sprite -> Assets
appendSprite assets path sprite =
  assets { sprites = Map.insert path sprite $ sprites assets }

{-|
  Performing the actual loading upon a @'LoadAssets'@.
-}
performAssetLoads :: LoadAssets -> IO Assets
performAssetLoads la =
  performAssetLoads' (values la) defaultAssets
  where performAssetLoads' :: [LoadAsset] -> Assets -> IO Assets
        performAssetLoads' []                   assets = return assets
        performAssetLoads' (LoadSprite path:xs) assets = (liftM (appendSprite assets path) $ loadSprite path) >>= performAssetLoads' xs

{-|
  Loading a @'Sprite'@ asset.
-}
loadSpriteAsset :: FilePath -> LoadAssets
loadSpriteAsset = fromValues . return . LoadSprite
