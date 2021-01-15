module Board where

data Mark = X | O | None
  deriving (Eq)
instance Show Mark where
  show X = "X"
  show O = "O"
  show None = "."

isMark :: Mark -> Bool
isMark None = False
isMark _    = True

isNone :: Mark -> Bool
isNone = not . isMark

opposite :: Mark -> Mark
opposite X = O
opposite O = X
opposite None = None


type Board = [[Mark]]

-- Get board size (rows, columns).
boardSize :: Board -> (Int, Int)
boardSize b@(r:_) = (length b, length r)

-- Pretty printer for board.
prettyBoard :: Board -> String
prettyBoard = concatMap ((++"\n") . (concatMap show))


-- Count marks for each row.
countMarks :: Board -> [(Int, Int, Int)]
countMarks = map (countMarks' (0, 0, 0))
  where
    countMarks' :: (Int, Int, Int) -> [Mark] -> (Int, Int, Int)
    countMarks' count [] = count
    countMarks' (x, o, none) (X:ms)    = countMarks' (x+1, o,   none)   ms
    countMarks' (x, o, none) (O:ms)    = countMarks' (x,   o+1, none)   ms
    countMarks' (x, o, none) (None:ms) = countMarks' (x,   o,   none+1) ms
