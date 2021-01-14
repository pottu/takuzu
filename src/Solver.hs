module Solver where

import Board
import Checker
import Lib
import Data.Maybe
import Data.List (transpose, group, elemIndices)


solve :: Board -> Maybe Board
solve b =
  let b' = applyTechniques b
   in case check b' of
        Correct   -> Just b'
        Incorrect -> Nothing
        Unknown   ->
          let guessX = solve $ guess X b'
              guessO = solve $ guess O b'
           in if isNothing guessX then guessO else guessX

-- Make a guess on first unmarked spot.
-- NOTE: Quick implementation. There probably is a better approach..
guess :: Mark -> Board -> Board
guess m b = 
  let len = length $ head b
   in chunksOf len $ guess' m $ concat b 
  where
    guess' :: Mark -> [Mark] -> [Mark]
    guess' m (None:ms) = m:ms
    guess' m (m':ms) = (m':guess' m ms) 

-- Applies techiques listed in ts recursively on the board 
-- (both rows and columns) until fixpoint is reached.
applyTechniques :: Board -> Board
applyTechniques b = 
  let b' = applyTechniques' ts $ transpose $ applyTechniques' ts $ transpose b
   in if b' == b then b else applyTechniques b'
  where
    -- TODO: Add techniques here.
    ts = [avoidingTriples1and2, avoidingTriples3, completeRow, advancedTechnique1]

    applyTechniques' :: [Board -> Board] -> Board -> Board
    applyTechniques' [] b = b
    applyTechniques' (t:ts) b = applyTechniques' ts $ t b



---- Techniques --------------------------------------------
avoidingTriples1and2 :: Board -> Board
avoidingTriples1and2 = map patterns
  where
    patterns :: [Mark] -> [Mark]
    -- Avoiding Triples 1
    patterns (None:O:O:ms) = (X:patterns (O:O:ms))
    patterns (None:X:X:ms) = (O:patterns (X:X:ms))
    patterns (O:O:None:ms) = (O:O:patterns (X:ms))
    patterns (X:X:None:ms) = (X:X:patterns (O:ms))
    -- Avoiding Triples 2
    patterns (O:None:O:ms) = (O:X:patterns (O:ms))
    patterns (X:None:X:ms) = (X:O:patterns (X:ms))
    -- No pattern matched, check rest.
    patterns (m:ms) = (m:patterns ms)
    patterns []     = []

completeRow :: Board -> Board
completeRow = map complete
  where
    complete :: [Mark] -> [Mark]
    complete row =
      let (xs, os) = countMarks row
          n = (length row) `div` 2
      in if xs == n
         then completeRow' O row
         else
           if os == n
           then completeRow' X row
           else row
      where
        countMarks :: [Mark] -> (Int, Int)
        countMarks [] = (0, 0)
        countMarks (X:ms) = let (x, o) = countMarks ms in (x+1, o)
        countMarks (O:ms) = let (x, o) = countMarks ms in (x, o+1)
        countMarks (_:ms) = countMarks ms

        completeRow' :: Mark -> [Mark] -> [Mark]
        completeRow' _ [] = []
        completeRow' m (None:ms) = (m:completeRow' m ms)
        completeRow' m (n:ms) = (n:(completeRow' m ms))

-- TODO: A lot of code shared between AdvTe1 and AvoTrip 3, refactor!
avoidingTriples3 :: Board -> Board
avoidingTriples3 b = zipWith aux (countMarks b) b
  where
    toPlace :: Int
    toPlace = (length $ head b) `div` 2

    aux :: (Int, Int, Int) -> [Mark] -> [Mark]
    aux (x, o, none) row | x + 1 == toPlace =
      let indices = elemIndices None row
       in tryPlace indices none X row
    aux (x, o, none) row | o + 1 == toPlace =
      let indices = elemIndices None row
       in tryPlace indices none O row
    aux _ row = row

    tryPlace :: [Int] -> Int -> Mark -> [Mark] -> [Mark]
    tryPlace [] _ _ row = row
    tryPlace (i:is) n mark row =
      let lastPlaced = replace i mark row
       in if hasTriples (fillWithOpposite lastPlaced)
             then replace i (opposite mark) row
             else tryPlace is n mark row
      where
        hasTriples row = any (\g -> (head g == (opposite mark)) && length g > 2) $ group row
        fillWithOpposite = map (\m -> if isMark m then m else opposite mark)

advancedTechnique1 :: Board -> Board
advancedTechnique1 b = zipWith findRow (countMarks b) b
  where
    toPlace :: Int
    toPlace = (length $ head b) `div` 2
    
    findRow :: (Int, Int, Int) -> [Mark] -> [Mark]
    findRow (x, o, none) row | o == toPlace - 2 = 
      let indices = elemIndices None row
       in tryPlace indices none O row
    findRow (x, o, none) row | x == toPlace - 2 =
      let indices = elemIndices None row
       in tryPlace indices none X row
    findRow _ row = row

    tryPlace :: [Int] -> Int -> Mark -> [Mark] -> [Mark]
    tryPlace [] _ _ row = row
    tryPlace (i:is) n mark row =
      let penultimate = replace i mark row
       in if placingLastResultsInTriples mark (n-1) penultimate
             then replace i (opposite mark) row
             else tryPlace is n mark row

    placingLastResultsInTriples :: Mark -> Int -> [Mark] -> Bool
    placingLastResultsInTriples mark nones row =
      and $ map (hasTriples . fillWithOpposite) placedLast
      where
        placedLast = [place mark n row | n <- [1..nones]]
        fillWithOpposite = map (\m -> if isMark m then m else opposite mark)
        hasTriples row = any (\g -> (head g == (opposite mark)) && length g > 2) $ group row

        place mark 1 (None:ms) = (mark:ms)
        place mark n (None:ms) = (None: place mark (n-1) ms)
        place mark n (m:ms)    = (m : place mark n ms)
