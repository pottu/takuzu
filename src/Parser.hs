module Parser where 

import Board

convert :: Char -> Mark
convert '.' = None
convert 'X' = X
convert 'O' = O

parse :: String -> Board
parse input = 
  let rows = tail $ lines input
   in map (map convert) rows
