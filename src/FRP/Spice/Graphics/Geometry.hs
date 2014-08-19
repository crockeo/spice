{-|
  This module provides a cleaner API to render a number of shapes in the
  current OpenGL context.
-}
module FRP.Spice.Graphics.Geometry where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Utils
import FRP.Spice.Math.Vector

----------
-- Code --

{-|
  Rendering a point.
-}
renderPoint :: Vector Float -> IO ()
renderPoint (Vector x y) = do
  renderPrimitive Points $
    vertex $ Vertex2 (togl x) (togl y)

{-|
  Rendering a rectangle.
-}
renderRectangle :: Vector Float -> Vector Float -> IO ()
renderRectangle (Vector x y) (Vector w h) = do
  renderPrimitive Quads $ do
    vertex $ Vertex2 (togl (x    )) (togl (y    ))
    vertex $ Vertex2 (togl (x + w)) (togl (y    ))
    vertex $ Vertex2 (togl (x + w)) (togl (y + h))
    vertex $ Vertex2 (togl (x    )) (togl (y + h))

{-|
  Rendering a square.
-}
renderSquare :: Vector Float -> Float -> IO ()
renderSquare pos size = renderRectangle pos $ Vector size size
