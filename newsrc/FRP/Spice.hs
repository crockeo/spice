{-|
  This module re-exports the core elements of the library, along with all of
  the types housed within the library.
-}
module FRP.Spice ( module Graphics.UI.GLFW
                 , module Data.Map.Strict

                 , module FRP.Spice.Internal.LoadAssets
                 , module FRP.Spice.Internal.Engine
                 , module FRP.Spice.Internal.Types
                 ) where

-------------------------
-- Re-exported Modules --
import Graphics.UI.GLFW ( MouseButton (..)
                        , SpecialKey (..)
                        , Key (..)
                        )
import Data.Map.Strict ((!))

import FRP.Spice.Internal.LoadAssets
import FRP.Spice.Internal.Engine
import FRP.Spice.Internal.Types
