{-|
  This module provides the ability to load assets (as of now, images and
  sounds) within the scope of the spice library.
-}
module FRP.Spice.Assets where

--------------------
-- Global Imports --
import qualified Data.Map.Strict as Map
import Control.Monad

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Sprite
import FRP.Spice.Utils.DoList

----------
-- Code --

{-|
  The container for the information necessary to load an asset.
-}
data LoadAsset = LoadImage FilePath

{-|
  The asset type itself.
-}
data Asset = Image

{-|
  A type alias for a DoList.
-}
type LoadAssets = DoList [LoadAsset]

{-|
  Loading an image @'Asset'@ in the form of a @'LoadAssets'@.
-}
loadImage :: FilePath -> LoadAssets
loadImage = fromValues . return . LoadImage

{-|
  Loading a single asset via a @'LoadAsset'@ call.
-}
loadAsset :: LoadAsset -> IO (FilePath, Asset)
loadAsset (LoadImage fp) = return (fp, Image)

{-|
  Constructing a Map of FilePath->Asset from a @'LoadAssets'@.
-}
loadAssets :: LoadAssets -> IO (Map.Map FilePath Asset)
loadAssets la =
  liftM (Map.fromList) $ sequence $ map loadAsset $ values la

{-|
  Converting from a map of @'Asset'@s to a map of @'Sprite'@s.
-}
toSprites :: Map.Map FilePath Asset -> Map.Map FilePath Sprite
toSprites = undefined
