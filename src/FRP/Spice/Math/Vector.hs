module FRP.Spice.Math.Vector where

--------------------
-- Global Imports --
import Data.Default
import Data.Monoid

----------
-- Code --

-- The vector datatype
data Vector a = Vector a a

-- Default instance
instance Default a => Default (Vector a) where
  def = Vector def def

-- Show instance
instance Show a => Show (Vector a) where
  show (Vector x y) = mconcat ["Vector ", show x, " ", show y]

-- Num instance
instance Num a => Num (Vector a) where
  (Vector x1 y1) + (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)
  (Vector x1 y1) * (Vector x2 y2) = Vector (x1 * x2) (y1 * y2)

  fromInteger n = Vector (fromInteger n) (fromInteger n)

  abs    (Vector x y) = Vector (abs    x) (abs    y)
  signum (Vector x y) = Vector (signum x) (signum y)
  negate (Vector x y) = Vector (negate x) (negate y)

-- Functor instance
instance Functor Vector where
  fmap fn (Vector x y) = Vector (fn x) (fn y)
