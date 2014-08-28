module FRP.Spice.Graphics.Sprite ( Sprite (..)
                                 , renderSprite
                                 , loadSprite
                                 ) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Control.Applicative
import Graphics.GLUtil
import Control.Monad

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Scene
import FRP.Spice.Graphics.Utils
import FRP.Spice.Math

----------
-- Code --

{-|
  Loading a @'TextureObject'@ from the filesystem.
-}
loadTex :: FilePath -> IO (TextureObject, Size)
loadTex fp = do
  t <- either error id <$> readTexture fp

  textureWrapMode Texture2D S $= (Repeated, ClampToBorder)
  textureWrapMode Texture2D T $= (Repeated, ClampToBorder)
  textureFilter   Texture2D   $= ((Linear', Just Linear'), Linear')
  textureBinding  Texture2D   $= Just t

  return (t, Size 50 50)

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
  texture Texture2D $= Enabled

  renderPrimitive Quads $
    forM_ (generateCoords pos $ spriteSize sprite) $ \(Vector x y) ->
      vertex $ Vertex2 (togl x) (togl y)

  texture Texture2D $= Disabled
  where generateCoords :: Vector Float -> Vector Float -> [Vector Float]
        generateCoords (Vector x y) (Vector w h) =
          [ Vector (x    ) (y    )
          , Vector (x + w) (y    )
          , Vector (x + w) (y + h)
          , Vector (x    ) (y + h)
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
