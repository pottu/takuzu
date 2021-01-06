module Generators where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Board

-- Generate arbitrary mark.
-- Gives higher frequency to None.
instance Arbitrary Mark where
  arbitrary = frequency [(5, return None), (1, return X), (1, return O)]

-- Generates an arbitrary board.
genBoard :: Gen Board
genBoard = do
  (Positive n) <- arbitrary
  (Positive m) <- arbitrary
  let nrRows = if even n then n else n+1
  let nrCols = if even m then m else m+1
  b <- vectorOf nrRows $ vectorOf nrCols arbitrary
  return b


-- Generates a small arbitrary board (2x2 to 10x10).
-- "Small" as in can be solved rather quickly.
-- FIXME: Increase size when more techniques are added..
genSmallBoard :: Gen Board
genSmallBoard = do
  nrRows <- elements [2,4..10]
  nrCols <- elements [2,4..10]
  b <- vectorOf nrRows $ vectorOf nrCols arbitrary
  return b

