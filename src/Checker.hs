module Checker where

import Board
import Data.List (transpose, group, nub)

-- Correct means board is fully and correctly marked.
-- Incorrect means some inconsistency has been detected.
-- Unknown means board is consitent but not yet filled.
data Result = Correct | Incorrect | Unknown deriving (Show, Eq)

-- Check if a board is correct.
check :: Board -> Result
check b = if checkPartial b
             then if allMarked b then Correct else Unknown
             else Incorrect

-- Check if a board is consistent (so far).
checkPartial :: Board -> Bool
checkPartial b =
  let b' = transpose b
   in and [maxTwoConsecutive b
          ,maxTwoConsecutive b'
          ,maxAmountOfMarks b
          ,maxAmountOfMarks b'
          ,allUnique b
          ,allUnique b'
          ]

-- Checks if board is fully marked.
allMarked :: Board -> Bool
allMarked = all (all isMark)

-- Checks if each row has at most two consecutive marks.
-- Run on transposed board to check columns.
maxTwoConsecutive :: Board -> Bool
maxTwoConsecutive b = 
  all (\g -> (not $ isMark $ head g) || length g <= 2) $ concatMap group b

-- Checks if each row contains at most half X's and O's.
-- Run on transposed board to check columns.
maxAmountOfMarks :: Board -> Bool
maxAmountOfMarks b =
  let n = (length $ head b) `div` 2
   in (all (\(x,o) -> x <= n && o <= n) $ map countMarks b)
  where
    countMarks :: [Mark] -> (Int, Int)
    countMarks [] = (0, 0)
    countMarks (X:ms) = let (x, o) = countMarks ms in (x+1, o)
    countMarks (O:ms) = let (x, o) = countMarks ms in (x, o+1)
    countMarks (_:ms) = countMarks ms 

-- Checks if all filled rows are unique.
-- Run on transposed board to check columns.
allUnique :: Board -> Bool
allUnique b =
  let b' = filter (all isMark) b
   in (length b') == (length $ nub b')
