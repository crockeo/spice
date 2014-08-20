module FRP.Spice.Graphics.Scene ( Scene
                                , fromRenderables
                                , renderScene
                                ) where

--------------------
-- Global Imports --
import Control.Applicative
import Control.Monad

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Renderable

----------
-- Code --

{-|
  For composing a scene out of a set of renderables.
-}
data SceneT a = SceneT [Render] a

{-|
  The commonly used instance of SceneT
-}
type Scene = SceneT ()

{-|
  Functor instance to satisfy applicative instance.
-}
instance Functor SceneT where
  fmap fn (SceneT renderables v) = SceneT renderables $ fn v

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
  (SceneT renderables v) >>= fn =
    let (SceneT renderables' v') = fn v in
      SceneT (renderables ++ renderables') v'

{-|
  Constructing a SceneT from a list of renderables.
-}
fromRenderables :: Renderable a => [a] -> Scene
fromRenderables renderables =
  SceneT (map toRender renderables) ()

{-|
  Rendering a whole @'Scene'@ (renders each @'Element'@ from first in list to last in
  list.)
-}
renderScene :: Scene -> IO ()
renderScene (SceneT renderables _) =
  forM_ renderables runRender
