{-|
  This module provides functions to load and manage the loading of assets.
-}
module FRP.Spice.Internal.Assets ( loadSpriteAsset
                                 , performAssetLoads
                                 ) where

--------------------
-- Global Imports --
import qualified Data.Map.Strict as Map
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Data.ByteString.Unsafe
import Control.Applicative
import Codec.Picture.Repa
import Codec.Picture
import Control.Monad
import Data.Default
import Foreign.Ptr

-------------------
-- Local Imports --
import FRP.Spice.Internal.Types

----------
-- Code --

{-|
  Loading a @'Sprite'@ from the file system from the @'FilePath'@ specified.
-}
loadSprite :: FilePath -> IO Sprite
loadSprite path = do
  wsize <- get windowSize
  liftM (makeSprite wsize) $ loadTexture path
  where getInfo :: DynamicImage -> Either String (Int, Int, PixelInternalFormat)
        getInfo (ImageRGB8   (Image w h _)) = Right (w, h, RGB8)
        getInfo (ImageRGB16  (Image w h _)) = Right (w, h, RGB16)
        getInfo (ImageRGBA8  (Image w h _)) = Right (w, h, RGBA8)
        getInfo (ImageRGBA16 (Image w h _)) = Right (w, h, RGBA16)
        getInfo _                           = Left "Unsupported image type."

        loadTexture :: FilePath -> IO (TextureObject, Size)
        loadTexture path = do
          img <- either error id <$> readImageRGBA path

          let dynimg         = imgToImage img
              (w, h, format) = either error id $ getInfo dynimg
              glSize         = TextureSize2D (fromIntegral w) (fromIntegral h)
              bs             = toByteString img

          ptr <- unsafeUseAsCString bs $ \cstr ->
            return $ castPtr cstr

          [t] <- genObjectNames 1

          textureBinding Texture2D $= Just t
          texImage2D Texture2D NoProxy 0 format glSize 0 (PixelData ABGR UnsignedByte ptr)

          return (t, Size (fromIntegral w) (fromIntegral h))

        makeSprite :: Size -> (TextureObject, Size) -> Sprite
        makeSprite (Size ww wh) (to, (Size w h)) =
          Sprite { spriteTex   = to
                 , spriteSize  = Vector ((fromIntegral w) / (fromIntegral ww))
                                        ((fromIntegral h) / (fromIntegral wh))
                 }

{-|
  Appending a @'Sprite'@ to an @'Assets'@.
-}
appendSprite :: Assets -> FilePath -> Sprite -> Assets
appendSprite assets path sprite =
  assets { sprites = Map.insert path sprite $ sprites assets }

{-|
  Creating a @'LoadAssets'@ from a call to load a sprite from a file.
-}
loadSpriteAsset :: FilePath -> LoadAssets
loadSpriteAsset path = DoListT [LoadSprite path] ()

{-|
  Performing all of the @'LoadAsset'@ commands contained in a @'LoadAssets'@.
-}
performAssetLoads :: LoadAssets -> IO Assets
performAssetLoads (DoListT las _) =
  performAssetLoads' las def
  where performAssetLoads' :: [LoadAsset] -> Assets -> IO Assets
        performAssetLoads' []                     assets = return assets
        performAssetLoads' ((LoadSprite path):xs) assets = (liftM (appendSprite assets path) $ loadSprite path) >>= performAssetLoads' xs
