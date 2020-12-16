module Parser where 

import Board
import qualified Data.Matrix as M (fromLists)

convert :: Char -> Mark
convert '.' = None
convert 'X' = X
convert 'O' = O

parse :: String -> Board
parse input = 
  let rows = tail $ lines input
   in M.fromLists $ map (map convert) rows
