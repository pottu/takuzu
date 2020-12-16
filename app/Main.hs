module Main where

import System.IO (getContents) 
import Parser (parse)
import Solver (solve)

main = do
  print "Okay, input a takuzu instance."
  instnc <- getContents
  print $ solve $ parse instnc

