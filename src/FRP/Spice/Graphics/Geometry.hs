{-|
  This module provides a cleaner API to render a number of shapes in the
  current OpenGL context.
-}
module FRP.Spice.Graphics.Geometry ( renderPoint
                                   , renderLine
                                   , renderRectangle
                                   , renderSquare
                                   , renderTriangle
                                   , renderPolygon
                                   ) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Control.Monad

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Scene
import FRP.Spice.Graphics.Utils
import FRP.Spice.Math.Vector

----------
-- Code --

{-|
  A version of @'renderPrimitive'@ where it takes a list of @'Vector'@
  @'Float'@s instead of performing @'vertex'@ calls.
-}
renderPrimitive' :: PrimitiveMode -> [Vector Float] -> Scene
renderPrimitive' mode points =
  renderPrimitive mode $
    forM_ points $ \(Vector x y) ->
      vertex $ Vertex2 (togl x) (togl y)

{-|
  Rendering a point.
-}
renderPoint :: Vector Float -> Scene
renderPoint pos = renderPrimitive' Points [pos]

{-|
  Rendering a line between two points.
-}
renderLine :: Vector Float -> Vector Float -> Scene
renderLine p1 p2 = renderPrimitive' Lines [p1, p2]

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
renderSquare pos size = renderRectangle pos $ Vector size size

{-|
  Rendering a triangle.
-}
renderTriangle :: Vector Float -> Vector Float -> Vector Float -> Scene
renderTriangle p1 p2 p3 = renderPrimitive' Triangles [p1, p2, p3]

{-|
  Rendering a polygon with 1-N vertecies.
-}
renderPolygon :: [Vector Float] -> Scene
renderPolygon l = renderPrimitive' Polygon l
