{-|
  This module provides a unified API for specifying every kind of rendering
  datatype and how they get rendered. All of this -- of course -- is only in
  the scope the spice project.
-}
module FRP.Spice.Graphics.Element ( Element (..)
                                  , renderElement
                                  ) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Control.Monad

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Utils
import FRP.Spice.Math

----------
-- Code --

{-|
  A type to purely contain the information for any kind of render call (that
  has been implemented so far.)
-}
data Element = RenderPrimitive PrimitiveMode [Vector Float]
             | SetColor Float Float Float Float

{-|
  Converting an @'Element'@ into its matching OpenGL call.
-}
renderElement :: Element -> IO ()
renderElement (RenderPrimitive mode vertecies) =
  renderPrimitive mode $
    forM_ vertecies $ \(Vector x y) ->
      vertex $ Vertex2 (togl x) (togl y)
renderElement (SetColor r g b a) =
  color $ Color4 (togl r) (togl g) (togl b) (togl a)
