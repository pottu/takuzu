module Solver where

import Board
import Checker
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
-- Chunkify list. 
chunksOf n [] = []
chunksOf n l = ((take n l):(chunksOf n $ drop n l))

-- Applies techiques listed in ts recursively on the board 
-- (both rows and columns) until fixpoint is reached.
applyTechniques :: Board -> Board
applyTechniques b = 
  let b' = applyTechniques' ts $ transpose $ applyTechniques' ts $ transpose b
   in if b' == b then b else applyTechniques b'
  where
    -- TODO: Add techniques here.
    ts = [avoidingTriples1and2, advancedTechnique1]

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

avoidTriples3 :: Board -> Board
avoidTriples3 = map f
  where
    f :: [Mark] -> [Mark]
    f row =
      let (xs, os) = countMarks row
          n = (length row) `div` 2
      in if xs == n - 1 && os == n - 2
         then placeInFirstSingleEmpty O row
         else
           if os == n - 1 && xs == n - 2
           then placeInFirstSingleEmpty X row
           else row
      where
        countMarks :: [Mark] -> (Int, Int)
        countMarks [] = (0, 0)
        countMarks (X:ms) = let (x, o) = countMarks ms in (x+1, o)
        countMarks (O:ms) = let (x, o) = countMarks ms in (x, o+1)
        countMarks (_:ms) = countMarks ms

        placeInFirstSingleEmpty :: Mark -> [Mark] -> [Mark]
        placeInFirstSingleEmpty _ [] = []
        placeInFirstSingleEmpty m [None] = [m]
        placeInFirstSingleEmpty X (None:X:None:ms) = (None:X:None:(placeInFirstSingleEmpty X ms))
        placeInFirstSingleEmpty O (None:O:None:ms) = (None:O:None:(placeInFirstSingleEmpty O ms))
        placeInFirstSingleEmpty X (None:None:X:ms) = (None:None:X:(placeInFirstSingleEmpty X ms))
        placeInFirstSingleEmpty O (None:None:O:ms) = (None:None:O:(placeInFirstSingleEmpty O ms))
        placeInFirstSingleEmpty X (X:None:None:ms) = (X:None:None:(placeInFirstSingleEmpty X ms))
        placeInFirstSingleEmpty O (O:None:None:ms) = (O:None:None:(placeInFirstSingleEmpty O ms))
        placeInFirstSingleEmpty m (O:None:X:ms) = (O:(placeInFirstSingleEmpty m (None:X:ms)))
        placeInFirstSingleEmpty m (X:None:O:ms) = (X:(placeInFirstSingleEmpty m (None:O:ms)))
        placeInFirstSingleEmpty m (n:None:ms) = (n:m:ms)
        placeInFirstSingleEmpty m (None:n:ms) = (m:n:ms)
        placeInFirstSingleEmpty m (n:ms) = (n:(placeInFirstSingleEmpty m ms))





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
     
---- Helper functions --------------------------------------
-- TODO: Place in lib file?

-- Replace element at index i with m in list.
replace :: Int -> a -> [a] -> [a]
replace n m l = let (l1, e:es) = splitAt n l in l1 ++ (m:es)

-- Count occurences of m in list.
count :: (Eq a) => a -> [a] -> Int
count m [] = 0
count m (m':ms) = if m == m' then 1 + count m ms else count m ms
