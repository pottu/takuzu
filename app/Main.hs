module Main where

import System.IO (getContents) 
import Parser (parse)
import Solver (solve)
import Board  (prettyBoard)
import Data.Maybe

main = do
  instnc <- getContents
  let sol = solve $ parse instnc
  case sol of
    Nothing  -> putStrLn "No solution."
    (Just s) -> putStr $ prettyBoard s
