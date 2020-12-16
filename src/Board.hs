module Board where

import Data.Matrix

data Mark = X | O | None
instance Show Mark where
  show X = "x"
  show O = "o"
  show None = "_"

isMark :: Mark -> Bool
isMark None = False
isMark _    = True

type Board = Matrix Mark

boardSize :: Board -> (Int, Int)
boardSize b = (nrows b, ncols b)
