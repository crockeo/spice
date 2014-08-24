{-|
  This module provides the ability to load assets (as of now, images and
  sounds) within the scope of the spice library.
-}
module FRP.Spice.Assets where

--------------------
-- Global Imports --
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as Map
import Codec.Picture
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
data Asset = ImageAsset ByteString
  deriving (Show)

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
loadAsset (LoadImage fp) = loadImageRaw fp

{-|
  Loading a raw image.
-}
loadImageRaw :: FilePath -> IO (FilePath, Asset)
loadImageRaw fp = do
  eImg <- readPng fp

  return $ case eImg of
    Left  err -> error err
    Right img ->
      case encodeDynamicPng img of
        Left  err -> error err
        Right bs  -> (fp, ImageAsset bs)

{-|
  Constructing a Map of FilePath->Asset from a @'LoadAssets'@.
-}
loadAssets :: LoadAssets -> IO (Map.Map FilePath Asset)
loadAssets la = liftM (Map.fromList) $ sequence $ map loadAsset $ values la
