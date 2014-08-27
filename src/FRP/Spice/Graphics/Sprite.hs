module FRP.Spice.Graphics.Sprite ( Sprite (..)
                                 , setActiveSprite
                                 , renderSprite
                                 , loadSprite
                                 ) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL
import Foreign.Marshal.Alloc
import Control.Applicative
import Foreign.Storable
import System.IO.Unsafe
import Graphics.GLUtil
import Control.Monad
import Data.IORef

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Scene
import FRP.Spice.Graphics.Utils
import FRP.Spice.Math

----------
-- Code --

{-|
  Getting a float from OpenGL
-}
getFloat :: GLenum -> IO Float
getFloat e = alloca $ \p -> do
  glGetFloatv e p
  liftM (fromgl) $ peek p

{-|
  A ref to count up from 0 -- used for counting up @'TextureUnit'@s.
-}
counterRef :: IORef Int
counterRef = unsafePerformIO $ newIORef 0

{-|
  Getting the next @'TextureUnit'@. Automatically ticks the @'counterRef'@ to
  go up one.
-}
nextTextureUnit :: IO TextureUnit
nextTextureUnit = do
  n <- readIORef counterRef
  writeIORef counterRef $ n + 1
  return $ TextureUnit $ fromIntegral n

{-|
  Loading a @'TextureObject'@ from the filesystem.
-}
loadTex :: FilePath -> IO (TextureObject, TextureUnit, Size)
loadTex fp = do
  t <- either error id <$> readTexture fp

  textureFilter Texture2D   $= ((Nearest, Nothing), Nearest)
  texture2DWrap             $= (Mirrored, ClampToEdge)

  tu <- nextTextureUnit

  return (t, tu, Size 50 50)

{-|
  A datatype to represent a @'TextureObject'@ through a reference to the
  @'TextureObject'@ itself and its @'Size'@.
-}
data Sprite = Sprite { spriteTex   :: TextureObject
                     , spriteTexId :: TextureUnit
                     , spriteSize  :: Vector Float
                     }

{-|
  Setting the @'Sprite'@ to be active.
-}
setActiveSprite :: Sprite -> IO ()
setActiveSprite sprite =
  activeTexture $= spriteTexId sprite

{-|
  Performing an OpenGL call to render the @'Sprite'@.
-}
renderSprite :: Sprite -> Vector Float -> Scene
renderSprite sprite pos = do
  setActiveSprite sprite
  renderPrimitive Quads $
    forM_ (generateCoords pos $ spriteSize sprite) $ \(Vector x y) ->
      vertex $ Vertex2 (togl x) (togl y)
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
makeSprite :: (TextureObject, TextureUnit, Size) -> Sprite
makeSprite (to, tu, (Size w h)) =
  Sprite { spriteTex   = to
         , spriteTexId = tu
         , spriteSize  = size
         }
  where size = Vector ((fromIntegral w) / 640) ((fromIntegral h) / 480)

{-|
  Loading a @'Sprite'@ from a file.
-}
loadSprite :: FilePath -> IO Sprite
loadSprite = liftM makeSprite . loadTex
