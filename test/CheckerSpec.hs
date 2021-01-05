module CheckerSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Board
import Checker

spec :: Spec
spec = do
  describe "allMarked" $ do
    it "identifies fully marked board" $ do
      allMarked [[X,O,O,X], [O,X,X,O]]
    it "detects not fully marked board 1" $ do
      not $ allMarked [[X,O,None,X], [O,X,X,O]]
    it "detects not fully marked board 2" $ do
      not $ allMarked [[X,O,O,X], [O,None,X,None]]
    it "detects completely unfilled board" $ do
      not $ allMarked [[None,None,None,None], [None,None,None,None]]

  describe "maxTwoConsecutive" $ do
    it "detects three-in-a-row X's" $ do
      not $ maxTwoConsecutive [[X,X,X,O], [O,X,O,X]]
    it "detects three-in-a-row O's 1" $ do
      not $ maxTwoConsecutive [[X,O,X,O], [X,O,O,O]]
    it "detects three-in-a-row O's 2" $ do
      not $ maxTwoConsecutive [[X,O,None,O,X,O], [O,X,None,O,O,O]]
    it "allows a None to break up three-in-a-row" $ do
      maxTwoConsecutive [[X,O,X,O,X,O], [X,X,O,O,None,O]]
    it "detects more than three-in-a-row marks 1" $ do
      not $ maxTwoConsecutive [[X,O,X,O,X,O], [O,X,O,O,O,O]]
    it "ignores three consecutive None's" $ do
      maxTwoConsecutive [[X,None,None,None], [X,O,O,X]]
    it "identifies completely unfilled board as correct" $ do
      maxTwoConsecutive [[None,None,None,None], [None,None,None,None]]

  describe "maxAmountOfMarks" $ do
    it "detects too many O's" $ do
      not $ maxAmountOfMarks [[X,X,O,O], [O,X,O,O]]
    it "detects too many X's" $ do
      not $ maxAmountOfMarks [[X,X,O,X], [O,X,O,O]]
    it "detects inconsistency in unfilled board" $ do
      not $ maxAmountOfMarks [[X,None,O,None], [O,None,O,O]]
    it "identifies a so-far consistent board" $ do
      maxAmountOfMarks [[X,None,O,None], [O,None,O,X]]
    it "identifies completely unfilled board as correct" $ do
      maxAmountOfMarks [[None,None,None,None], [None,None,None,None]]

  describe "allUnique" $ do
    it "detects duplicate rows 1" $ do
      not $ allUnique [[X,O,X,O], [X,O,X,O]]
    it "detects duplicate rows 2" $ do
      not $ allUnique [[X,O,X,O], [X,O,O,X], [X,O,X,O], [None,O,None,O]]
    it "ignores duplicate unfilled rows" $ do
      allUnique [[X,O,X,None], [X,O,O,X], [X,O,X,O], [X,O,X,None]]
    it "identifies completely unfilled board as correct" $ do
      allUnique [[None,None,None,None], [None,None,None,None]]
