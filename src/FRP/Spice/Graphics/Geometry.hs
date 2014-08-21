{-|
  This module provides a cleaner API to render a number of shapes in the
  current OpenGL context.
-}
module FRP.Spice.Graphics.Geometry ( renderPoint
                                   , renderRectangle
                                   , renderSquare
                                   ) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Element
import FRP.Spice.Graphics.Scene
import FRP.Spice.Math.Vector

----------
-- Code --

{-|
  Rendering a point.
-}
renderPoint :: Vector Float -> Scene
renderPoint pos =
  fromElements [RenderPrimitive Points [pos]]

{-|
  Rendering a rectangle.
-}
renderRectangle :: Vector Float -> Vector Float -> Scene
renderRectangle (Vector x y) (Vector w h) = do
  fromElements [ RenderPrimitive Quads [ Vector (x    ) (y    )
                                       , Vector (x + w) (y    )
                                       , Vector (x + w) (y + h)
                                       , Vector (x    ) (y + h)
                                       ]
               ]

{-|
  Rendering a square.
-}
renderSquare :: Vector Float -> Float -> Scene
renderSquare pos size = renderRectangle pos $ Vector size size
