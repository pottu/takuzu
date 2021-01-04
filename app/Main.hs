module Main where

import System.IO (getContents) 
import Parser (parse)
import Solver (solve)
import Board  (prettyBoard)
import Data.Maybe

main = do
  putStrLn "Usage: cat instances/N-M.txt | stack run"
  instnc <- getContents
  putStrLn "Okay, your instance is:"
  putStr instnc
  putStrLn "Here - I solved it for you:"
  -- FIXME: Assumes solved board.
  putStr $ prettyBoard $ fromJust $ solve $ parse instnc

