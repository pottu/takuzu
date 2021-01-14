module SolverSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Board
import Generators
import Solver
import Checker
import Data.Maybe (isJust, fromJust)


spec :: Spec
spec = do
  describe "solve" $ do
    it "produces only witnesses that passes the checker" $
      forAll (fmap solve genSmallBoard `suchThat` isJust)
        (\b -> (check $ fromJust b) `shouldBe` Correct)

    it "does not remove any marks initially on the board" $
      forAll genSmallBoard (\b -> 
        let sameMarks :: Board -> Board -> Bool
            sameMarks b b' = 
              all (\(m, m') -> not (isMark m) || m == m') $ 
                  zip (concat b) (concat b')
            b' :: Maybe Board
            b' = solve b 
         in isJust b' ==> sameMarks b (fromJust b'))
  describe "avoidTriples3" $ do
    it "places the last of a specific mark on a row such the other marks don't form a triple" $
      avoidTriples3 [[None, X, O, X, None, O, None, None],
                      [None, None, None, O, None, X, O, X],
                      [O, O, X, X, O, X, X, O],
                      [X, None, None, None, X, O, O, X],
                      [None, None, None, O, X, X, O, None],
                      [None, X, None, None, O, X, X, O],
                      [None, None, None, X, None, O, None, None],
                      [None, None, None, None, None, O, None, None]]
      `shouldBe`
      [[None, X, O, X, None, O, None, None],
        [None, None, None, O, None, X, O, X],
        [O, O, X, X, O, X, X, O],
        [X, None, None, None, X, O, O, X],
        [None, None, None, O, X, X, O, None],
        [O, X, None, None, O, X, X, O],
        [None, None, None, X, None, O, None, None],
        [None, None, None, None, None, O, None, None]]
  describe "completeRow" $ do
    it "completes a row if only a single mark is missing" $
      completeRow [[None, None, O, None, None, None, O, None],
                    [None, O, X, O, O, X, None, X],
                    [None, None, None, X, None, O, X, O],
                    [None, None, O, X, X, O, None, X],
                    [None, X, None, O, None, X, None, None],
                    [O, None, X, O, O, X, None, None],
                    [X, None, None, X, X, None, X, None],
                    [X, None, None, None, None, None, None, None]]
      `shouldBe`
      [[None, None, O, None, None, None, O, None],
        [None, O, X, O, O, X, None, X],
        [None, None, None, X, None, O, X, O],
        [None, None, O, X, X, O, None, X],
        [None, X, None, O, None, X, None, None],
        [O, None, X, O, O, X, None, None],
        [X, O, O, X, X, O, X, O],
        [X, None, None, None, None, None, None, None]]
