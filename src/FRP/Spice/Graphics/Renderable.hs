module FRP.Spice.Graphics.Renderable where

----------
-- Code --

{-|
  A container around IO to suggest that one should perform rendering in a
  rendering function.
-}
newtype Render = Render (IO ())

{-|
  Performing the IO contained in a @'Render'@.
-}
runRender :: Render -> IO ()
runRender (Render action) = action

{-|
  A class that provides an API to convert a datatype into a @'Render'@.
-}
class Renderable a where
  toRender :: a -> Render
