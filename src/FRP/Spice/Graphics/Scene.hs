{-|
  This module provides an API to compose @'Scene'@s through do-notation.
-}
module FRP.Spice.Graphics.Scene where

----------
-- Code --

{-|
  A type synonym for a single IO () call to suggest that users should be
  *rendering* in render calls, and not performing other IO.
-}
type Scene = IO ()
