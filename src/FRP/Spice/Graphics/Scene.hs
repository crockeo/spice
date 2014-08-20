module FRP.Spice.Graphics.Scene where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Control.Applicative
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
  For composing a scene out of a set of elements.
-}
data SceneT a = SceneT [Element] a

{-|
  The commonly used instance of SceneT
-}
type Scene = SceneT ()

{-|
  Functor instance to satisfy applicative instance.
-}
instance Functor SceneT where
  fmap fn (SceneT elements v) = SceneT elements $ fn v

{-|
  Applicative instance to satisfy the monad instance. Not advised to use
-}
instance Applicative SceneT where
  pure  = return
  (<*>) = ap

{-|
  Used for being able to compose Scenes in a do-notation. Not very useful
  outside of that.
-}
instance Monad SceneT where
  return a = SceneT [] a
  (SceneT elements v) >>= fn =
    let (SceneT elements' v') = fn v in
      SceneT (elements ++ elements') v'

{-|
  Rendering a single @'Element'@.
-}
renderElement :: Element -> IO ()
renderElement (Element pm vs) =
  renderPrimitive pm $
    forM_ vs $ \(Vector x y) ->
      vertex $ Vertex2 (togl x) (togl y)

{-|
  Rendering a whole @'Scene'@ (renders each @'Element'@ from first in list to last in
  list.)
-}
renderScene :: Scene -> IO ()
renderScene (SceneT elements _) =
  forM_ elements renderElement
