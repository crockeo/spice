{-|
  This module provides a @'Vector'@ datatype as described below.
-}
module FRP.Spice.Math.Vector where

--------------------
-- Global Imports --
import Data.Default
import Data.Monoid

----------
-- Code --

{-|
  A datatype that houses two values of a given type. It is provided with a
  @'Num'@ instance so that, when used with number types it can function
  similarly (though not exactly) to a mathematical vector.
-}
data Vector a = Vector a a

{-|
  The default for the @'Vector'@.
-}
instance Default a => Default (Vector a) where
  def = Vector def def

{-|
  Displaying the @'Vector'@.
-}
instance Show a => Show (Vector a) where
  show (Vector x y) = mconcat ["Vector ", show x, " ", show y]

{-|
  Performs operations on the matching fields of the other @'Vector'@.
-}
instance Num a => Num (Vector a) where
  (Vector x1 y1) + (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)
  (Vector x1 y1) * (Vector x2 y2) = Vector (x1 * x2) (y1 * y2)

  fromInteger n = Vector (fromInteger n) (fromInteger n)

  abs    (Vector x y) = Vector (abs    x) (abs    y)
  signum (Vector x y) = Vector (signum x) (signum y)
  negate (Vector x y) = Vector (negate x) (negate y)

{-|
  Maps over both values in the @'Vector'@
-}
instance Functor Vector where
  fmap fn (Vector x y) = Vector (fn x) (fn y)
