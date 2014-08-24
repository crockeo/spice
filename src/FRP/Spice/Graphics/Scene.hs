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

fromElements :: [Element] -> Scene
fromElements = fromValues

renderScene :: Scene -> IO ()
renderScene scene =
  mapM_ renderElement $ values scene
