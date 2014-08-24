{-|
  This module provides an API to compose @'Scene'@s through do-notation.
-}
module FRP.Spice.Graphics.Scene where

-------------------
-- Local Imports --
import FRP.Spice.Graphics.Element
import FRP.Spice.Utils.DoList

----------
-- Code --

{-|
  A DoList to compose a list of @'Element'@s to render using do-notation.
-}
type Scene = DoList [Element]

{-|
  Converting a list of @'Element'@s into a @'Scene'@ so that it may be composed
  through do-notation.
-}
fromElements :: [Element] -> Scene
fromElements = fromValues

{-|
  Rendering a Scene (effectively the same as running @'renderelement'@ on a
  list of @'Element'@s.)
-}
renderScene :: Scene -> IO ()
renderScene = mapM_ renderElement . values
