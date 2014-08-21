module FRP.Spice.Graphics.Scene where

--------------------
-- Global Imports --
import Control.Applicative
import Control.Monad

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Element

----------
-- Code --

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
  Constructing a SceneT from a list of elements.
-}
fromElements :: [Element] -> Scene
fromElements elements =
  SceneT elements ()

{-|
  Rendering a whole @'Scene'@ (renders each @'Element'@ from first in list to last in
  list.)
-}
renderScene :: Scene -> IO ()
renderScene (SceneT elements _) =
  forM_ elements renderElement
