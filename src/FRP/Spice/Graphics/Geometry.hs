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
import Graphics.Rendering.OpenGL ( PrimitiveMode (..)
                                 , Vertex2 (..)
                                 , renderPrimitive
                                 , vertex
                                 )
import Control.Monad

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Renderable
import FRP.Spice.Graphics.Scene
import FRP.Spice.Graphics.Utils
import FRP.Spice.Math.Vector

----------
-- Code --

data Primitive = Primitive PrimitiveMode [Vector Float]

instance Renderable Primitive where
  toRender (Primitive mode vertecies) =
    Render $
      renderPrimitive mode $
        forM_ vertecies $ \(Vector x y) ->
          vertex $ Vertex2 (togl x) (togl y)

{-|
  Rendering a point.
-}
renderPoint :: Vector Float -> Scene
renderPoint pos =
  fromRenderables [Primitive Points [pos]]

{-|
  Rendering a rectangle.
-}
renderRectangle :: Vector Float -> Vector Float -> Scene
renderRectangle (Vector x y) (Vector w h) = do
  fromRenderables [ Primitive Quads [ Vector (x    ) (y    )
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
