module Board where

data Mark = X | O | None
  deriving (Eq)
instance Show Mark where
  show X = "X"
  show O = "O"
  show None = "·"

isMark :: Mark -> Bool
isMark None = False
isMark _    = True

type Board = [[Mark]]

-- Get board size (rows, columns).
boardSize :: Board -> (Int, Int)
boardSize b@(r:_) = (length b, length r)

-- Pretty printer for board.
prettyBoard :: Board -> String
prettyBoard b = concatMap ((++"\n") . (concatMap show)) b


-- NOTE: Currently unused.
data BoardStatus = BoardStatus 
                   { transposed :: Bool
                   , nrOfRows :: Int
                   , nrOfCols :: Int
                   , nrOfMarks :: [(Int, Int, Int)]
                   }

boardStatus :: Board -> Bool -> Int -> Int -> BoardStatus
boardStatus b t r c = BoardStatus
  { transposed = t
  , nrOfRows = r
  , nrOfCols = c
  , nrOfMarks = countMarks b
  }

countMarks :: Board -> [(Int, Int, Int)]
countMarks = map (countMarks' (0, 0, 0))
  where
    countMarks' :: (Int, Int, Int) -> [Mark] -> (Int, Int, Int)
    countMarks' count [] = count
    countMarks' (x, o, none) (X:ms)    = countMarks' (x+1, o,   none)   ms
    countMarks' (x, o, none) (O:ms)    = countMarks' (x,   o+1, none)   ms
    countMarks' (x, o, none) (None:ms) = countMarks' (x,   o,   none+1) ms
