module FRP.Spice.Graphics.Element where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Control.Monad

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Utils
import FRP.Spice.Math.Vector

----------
-- Code --

{-|
  Purely specifying the rendering behavior of a single element. To be composed
  into @'FRP.Spice.Graphics.Scene'@s for a full rendering effect.
-}
data Element = Element PrimitiveMode [Vector Float]

{-|
  Rendering a single @'Element'@.
-}
renderElement :: Element -> IO ()
renderElement (Element pm vs) =
  renderPrimitive pm $
    forM_ vs $ \(Vector x y) ->
      vertex $ Vertex2 (togl x) (togl y)
