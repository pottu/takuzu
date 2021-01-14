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

avoidDuplication :: Board -> Board
avoidDuplication b =
  let completed = filter isCompleted b
  in if length completed == 0
  then b
  else undefined
  where
    isCompleted :: [Mark] -> Bool
    isCompleted row =
      let (xs, os) = countMarks row
          n = (length row) `div` 2
      in xs == n && os == n

    countMarks :: [Mark] -> (Int, Int)
    countMarks [] = (0, 0)
    countMarks (X:ms) = let (x, o) = countMarks ms in (x+1, o)
    countMarks (O:ms) = let (x, o) = countMarks ms in (x, o+1)
    countMarks (_:ms) = countMarks ms

    almostIdentical :: [Mark] -> [Mark] -> Bool
    almostIdentical [] [] = True
    almostIdentical (None:xs) (_:ys) = almostIdentical xs ys
    almostIdentical (_:xs) (None:ys) = almostIdentical xs ys
    almostIdentical (x:xs) (y:ys) = x == y && almostIdentical xs ys
  
  -- let n = length b
  --     rs = [(b !! x, b !! y) | x <- [0..(n-1)], y <- [0..(n-1)], x < y]
  -- in
  --   uniq $ map avoidDuplication' rs
  --   where
  --     avoidDuplication' :: ([Mark], [Mark]) -> [Mark]
  --     avoidDuplication' (x, y) =
  --       let (axs, ays) = countMarks x
  --           (bxs, bys) = countMarks y
  --           n = (length x) `div` 2
  --       in if (axs + ays == n - 2) && (bxs + bys == n) && (almostIdentical x y) && (x /= y)
  --          then complete x y
  --          else x
  --       where
  --         countMarks :: [Mark] -> (Int, Int)
  --         countMarks [] = (0, 0)
  --         countMarks (X:ms) = let (x, o) = countMarks ms in (x+1, o)
  --         countMarks (O:ms) = let (x, o) = countMarks ms in (x, o+1)
  --         countMarks (_:ms) = countMarks ms

  --         almostIdentical :: [Mark] -> [Mark] -> Bool
  --         almostIdentical [] [] = True
  --         almostIdentical (None:xs) (_:ys) = almostIdentical xs ys
  --         almostIdentical (_:xs) (None:ys) = almostIdentical xs ys
  --         almostIdentical (x:xs) (y:ys) = x == y && almostIdentical xs ys

  --         complete :: [Mark] -> [Mark] -> [Mark]
  --         complete [] [] = []
  --         complete (None:xs) (X:ys) = (O:complete xs ys)
  --         complete (None:xs) (O:ys) = (X:complete xs ys)
  --         complete (_:xs) (y:ys) = (y:complete xs ys)        

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

-- Chunkify list. 
chunksOf n [] = []
chunksOf n l = ((take n l):(chunksOf n $ drop n l))
