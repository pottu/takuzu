module Lib where

import Data.List

-- Replace element at index i with m in list.
replace :: Int -> a -> [a] -> [a]
replace n m l = let (l1, e:es) = splitAt n l in l1 ++ (m:es)

-- Count occurences of m in list.
count :: (Eq a) => a -> [a] -> Int
count m [] = 0
count m (m':ms) = if m == m' then 1 + count m ms else count m ms

-- Chunkify list. 
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n l = ((take n l):(chunksOf n $ drop n l))

