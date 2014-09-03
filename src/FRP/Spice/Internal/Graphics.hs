{-|

-}
module FRP.Spice.Internal.Graphics ( bindColor
                                   , color4f
                                   , color3f
                                   , color4i
                                   , color3i
                                   , black
                                   , white
                                   , red
                                   , green
                                   , blue
                                   , renderPoint
                                   , renderLine
                                   , renderTriangle
                                   , renderRectangle
                                   , renderSquare
                                   , renderPolygon
                                   , renderSprite
                                   , renderSpriteWithSize
                                   ) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding (Color)
import Control.Monad

-------------------
-- Local Imports --
import FRP.Spice.Internal.Types

----------
-- Code --

{-|
  Converting a @'Float'@ to a @'GLfloat'@.
-}
toGL :: Float -> GLfloat
toGL = realToFrac

{-|
  Binding a color to change the current OpenGL color.
-}
bindColor :: Color -> Scene
bindColor (Color r g b a) = color $ Color4 (toGL r) (toGL g) (toGL b) (toGL a)

{-|
  Constructing a color from 4 @'Float'@s.
-}
color4f :: Float -> Float -> Float -> Float -> Color
color4f = Color

{-|
  Constructing a color from 3 @'Float'@s, with the alpha channel defaulting to
  its maximum (of 1.0).
-}
color3f :: Float -> Float -> Float -> Color
color3f r g b = color4f r g b 1.0

{-|
  Constructing a color from 4 @'Int'@s.
-}
color4i :: Int -> Int -> Int -> Int -> Color
color4i r g b a =
  color4f (fromIntegral r / 255)
          (fromIntegral g / 255)
          (fromIntegral b / 255)
          (fromIntegral a / 255)

{-|
  Constructing a color from 3 @'Int'@s, with the alpha channel defaulting to
  its maximum (of 255).
-}
color3i :: Int -> Int -> Int -> Color
color3i r g b = color4i r g b 255

{-|
  The color black.
-}
black :: Color
black = color3i 0 0 0

{-|
  The color white.
-}
white :: Color
white = color3i 255 255 255

{-|
  The color gray.
-}
grey :: Color
grey = color3i 255 255 255

{-|
  A synonym for the color grey.
-}
gray :: Color
gray = grey

{-|
  The color red.
-}
red :: Color
red = color3i 255 0 0

{-|
  The color green.
-}
green :: Color
green = color3i 0 255 0

{-|
  The color blue.
-}
blue :: Color
blue = color3i 0 0 255

{-|
  Rendering a primitive.
-}
renderPrimitive' :: PrimitiveMode -> [Vector Float] -> Scene
renderPrimitive' pm l =
  renderPrimitive pm $
    forM_ l $ \(Vector x y) ->
      vertex $ Vertex2 (toGL x) (toGL y)

{-|
  Rendering a position.
-}
renderPoint :: Vector Float -> Scene
renderPoint = renderPrimitive' Points . return

{-|
  Rendering a line.
-}
renderLine :: Vector Float -> Vector Float -> Scene
renderLine p1 p2 = renderPrimitive' Lines [p1, p2]

{-|
  Rendering a triangle.
-}
renderTriangle :: Vector Float -> Vector Float -> Vector Float -> Scene
renderTriangle p1 p2 p3 = renderPrimitive' Triangles [p1, p2, p3]

{-|
  Rendering a rectangle.
-}
renderRectangle :: Vector Float -> Vector Float -> Scene
renderRectangle (Vector x y) (Vector w h) =
  renderPrimitive' Quads [ Vector (x    ) (y    )
                         , Vector (x + w) (y    )
                         , Vector (x + w) (y + h)
                         , Vector (x    ) (y + h)
                         ]

{-|
  Rendering a square.
-}
renderSquare :: Vector Float -> Float -> Scene
renderSquare pos x = renderRectangle pos $ Vector x x

{-|
  Rendering a polygon of any n sides.
-}
renderPolygon :: [Vector Float] -> Scene
renderPolygon l = renderPrimitive' Polygon l

{-|
  Rendering a @'Sprite'@ at the position specified.
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
      texCoord $ TexCoord2 (toGL tx) (toGL ty)
      vertex   $ Vertex2   (toGL  x) (toGL  y)

  texture Texture2D        $= Disabled
  where generateCoords :: Vector Float -> Vector Float -> [(Vector Float, Vector Float)]
        generateCoords (Vector x y) (Vector w h) =
          [ (Vector (x    ) (y    ), Vector 0 0)
          , (Vector (x + w) (y    ), Vector 1 0)
          , (Vector (x + w) (y + h), Vector 1 1)
          , (Vector (x    ) (y + h), Vector 0 1)
          ]

{-|
  Rendering a @'Sprite'@ at the position specified with the size specified.
-}
renderSpriteWithSize :: Sprite -> Vector Float -> Vector Float -> Scene
renderSpriteWithSize sprite pos size =
  renderSprite (sprite { spriteSize = size }) pos
