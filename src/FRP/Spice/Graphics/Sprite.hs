{-|
  This module provides an API for loading and rendering textures in the form
  of @'Sprite'@s.
-}
module FRP.Spice.Graphics.Sprite ( Sprite (..)
                                 , renderSprite
                                 , loadSprite
                                 ) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Data.ByteString.Unsafe
import Control.Applicative
import Codec.Picture.Repa
import Codec.Picture
import Control.Monad
import Foreign.Ptr

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Color
import FRP.Spice.Graphics.Scene
import FRP.Spice.Graphics.Utils
import FRP.Spice.Math

----------
-- Code --

{-|
  Getting the size from a @'DynamicImage'@.
-}
getInfo :: DynamicImage -> (Int, Int, PixelInternalFormat)
getInfo (ImageRGB8   (Image w h _)) = (w, h, RGB8)
getInfo (ImageRGB16  (Image w h _)) = (w, h, RGB16)
getInfo (ImageRGBA8  (Image w h _)) = (w, h, RGBA8)
getInfo (ImageRGBA16 (Image w h _)) = (w, h, RGBA16)

{-|
  Loading a @'TextureObject'@ and @'Size'@ from any RGB8, RGB16, RGBA8, or
  RGBA16 images.
-}
loadTex :: FilePath -> IO (TextureObject, Size)
loadTex path = do
  img <- either error id <$> readImageRGBA path

  let dynimg         = imgToImage img
      (w, h, format) = getInfo dynimg
      glSize         = TextureSize2D (fromIntegral w) (fromIntegral h)
      bs             = toByteString img

  ptr <- unsafeUseAsCString bs $ \cstr ->
    return $ castPtr cstr

  [t] <- genObjectNames 1

  textureBinding Texture2D $= Just t
  texImage2D Texture2D NoProxy 0 format glSize 0 (PixelData ABGR UnsignedByte ptr)

  return (t, Size (fromIntegral w) (fromIntegral h))

{-|
  A datatype to represent a @'TextureObject'@ through a reference to the
  @'TextureObject'@ itself and its @'Size'@.
-}
data Sprite = Sprite { spriteTex   :: TextureObject
                     , spriteSize  :: Vector Float
                     }

{-|
  Performing an OpenGL call to render the @'Sprite'@.
-}
renderSprite :: Sprite -> Vector Float -> Scene
renderSprite sprite pos = do
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureFunction             $= Replace

  texture        Texture2D $= Enabled
  textureBinding Texture2D $= (Just $ spriteTex sprite)

  renderPrimitive Quads $
    forM_ (generateCoords pos $ spriteSize sprite) $ \(Vector x y, Vector tx ty) -> do
      texCoord $ TexCoord2 (togl tx) (togl ty)
      vertex $ Vertex2 (togl x) (togl y)

  texture Texture2D        $= Disabled
  where generateCoords :: Vector Float -> Vector Float -> [(Vector Float, Vector Float)]
        generateCoords (Vector x y) (Vector w h) =
          [ (Vector (x    ) (y    ), Vector 0 0)
          , (Vector (x + w) (y    ), Vector 1 0)
          , (Vector (x + w) (y + h), Vector 1 1)
          , (Vector (x    ) (y + h), Vector 0 1)
          ]

{-|
  Creating a @'Sprite'@ from a @'TextureObject'@.
-}
makeSprite :: (TextureObject, Size) -> Sprite
makeSprite (to, (Size w h)) =
  Sprite { spriteTex   = to
         , spriteSize  = size
         }
  where size = Vector ((fromIntegral w) / 640) ((fromIntegral h) / 480)

{-|
  Loading a @'Sprite'@ from a file.
-}
loadSprite :: FilePath -> IO Sprite
loadSprite = liftM makeSprite . loadTex
