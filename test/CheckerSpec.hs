module CheckerSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Board
import Checker

board01 = [ [X,X,O,O]
          , [X,O,X,O]
          , [O,O,X,X]
          , [O,X,O,X]
          ]

spec :: Spec
spec = do
  describe "allMarked" $ do
    it "identifies fully marked board" $ do
      allMarked [ [X,O,O,X]
                , [O,X,X,O] ]
    it "detects not fully marked board" $ do
      not $ allMarked [ [X,O,None,X]
                      , [O,X,X,O] ]
    it "detects not fully marked board" $ do
      not $ allMarked [ [X,O,O,X]
                      , [O,None,X,O] ]
    it "detects not fully marked board" $ do
      not $ allMarked [ [None,None,None,None]
                      , [None,None,None,None] ]

  describe "maxTwoConsecutive" $ do
    it "detects three-in-a-row X's" $ do
      not $ maxTwoConsecutive [[X,X,X,O], [O,X,O,X]]
    it "detects three-in-a-row O's" $ do
      not $ maxTwoConsecutive [[X,O,X,O], [X,O,O,O]]
    it "detects more than three-in-a-row marks" $ do
      not $ maxTwoConsecutive [[X,O,X,O,X,O], [O,X,O,O,O,O]]

  describe "sameNumberOfMarks" $ do
    it "detects more X's than O's" $ do
      not $ sameNumberOfMarks [[X,X,O,O], [O,X,O,O]]

  describe "allUnique" $ do
    it "detects non-unique rows" $ do
      not $ allUnique [[X,O,X,O], [X,O,X,O]]
