{-|
  This module re-exports some other modules in the spice library so that you
  needn't import all of them explicitly.
-}
module FRP.Spice ( module FRP.Spice.Config
                 , module FRP.Spice.Engine
                 , module FRP.Spice.Input
                 , module FRP.Spice.Game
                 ) where

-----------------------
-- Rexported Imports --
import FRP.Spice.Config
import FRP.Spice.Engine
import FRP.Spice.Input
import FRP.Spice.Game
