module Parser where 

import Board

convert :: Char -> Mark
convert '.' = None
convert 'X' = X
convert 'O' = O
convert  _  = error "Invalid characters in board. Only 'X', 'O' or '.' allowed."

-- Parse a board from an input file.
parse :: String -> Board
parse input = 
  let rows   = tail $ lines input
      nrRows = length rows
      nrCols = length $ head rows
      b      = map (map convert) rows
   in if even nrRows && even nrCols && all (\r -> length r == nrCols) rows
         then b
         else error "Invalid board size."
