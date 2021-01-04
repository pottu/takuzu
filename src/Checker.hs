module Checker where

import Board
import Data.List (transpose, group, nub)

data Result = Correct | Incorrect | Unknown

-- TODO: I think a partial checker might come in handy.
-- Either a separate checker or extend this one.. Hm..
check :: Board -> Result
check b = 
  let b' = transpose b
   in if allMarked b
         then checkComplete b
         else Unknown

-- Expects a fully marked board.
checkComplete :: Board -> Result
checkComplete b =
  let b' = transpose b
   in if and [maxTwoConsecutive b
             ,maxTwoConsecutive b'
             ,sameNumberOfMarks b
             ,sameNumberOfMarks b'
             ,allUnique b
             ,allUnique b'
             ]
          then Correct else Incorrect

-- Checks if board is fully marked.
allMarked :: Board -> Bool
allMarked = all (all isMark)

-- Checks if each row has at most two consecutive marks.
-- Run on transposed board to check columns.
maxTwoConsecutive :: Board -> Bool
maxTwoConsecutive b = 
  all (\g -> length g <= 2) $ concatMap group b

-- Checks if each row contains the an equal number of X's and O's.
-- Assumes board is fully marked.
-- Run on transposed board to check columns.
sameNumberOfMarks :: Board -> Bool
sameNumberOfMarks b =
  let n = (length $ head b) `div` 2 -- Expected amount of X's/O's per row.
   in all (n ==) $ map (count 0 X) b
  where
    count :: Eq a => Int -> a -> [a] -> Int
    count n _ [] = n
    count n e (l:ls) = 
      if e == l 
         then count (n+1) e ls
         else count n e ls

-- Checks if all rows are unique.
-- Run on transposed board to check columns.
allUnique :: Board -> Bool
allUnique b = (length b) == (length $ nub b)
