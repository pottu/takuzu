module SolverSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck (modifyMaxSuccess)

import Board
import Generators
import Solver
import Checker
import Data.Maybe (isJust, fromJust)

-- Check if b' has all marks that b has.
sameMarks :: Board -> Board -> Bool
sameMarks b b' = 
  all (\(m, m') -> not (isMark m) || m == m') $ zip (concat b) (concat b')


spec :: Spec
spec = do
  describe "solve" $ do
    it "produces only witnesses that passes the checker" $
      forAll (fmap solve genSmallBoard `suchThat` isJust)
        (\b -> (check $ fromJust b) `shouldBe` Correct)

    it "does not remove any marks initially on the board" $
      forAll genSmallBoard (\b -> 
        let b' = solve b 
         in isJust b' ==> sameMarks b (fromJust b'))

    it "manages to solve solvable boards" $
      forAll genSolvableBoard (\b ->
        let b' = solve b
         in check (fromJust b') `shouldBe` Correct)

  describe "avoidingTriples1and2" $ do
    it "mimics given example 1" $ do
      let example = [[None,None,None,X,X,None,None,None],
                     [None,None,None,None,None,O,O,None],
                     [None,None,X,None,None,O,O,None],
                     [X,O,X,O,X,O,X,O]]
          result  = [[None,None,O,X,X,O,None,None],
                     [None,None,None,None,X,O,O,X],
                     [None,None,X,None,X,O,O,X],
                     [X,O,X,O,X,O,X,O]]
      avoidingTriples1and2 example `shouldBe` result
    it "mimics given example 2" $ do
      let example = [[None,None,None,X,None,X,None,None],
                     [O,None,O,None,None,O,None,None]]
          result  = [[None,None,None,X,O,X,None,None],
                     [O,X,O,None,None,O,None,None]]
      avoidingTriples1and2 example `shouldBe` result
    it "does not remove any marks initially on the board" $
      forAll genBoard (\b -> sameMarks b (avoidingTriples1and2 b))

  describe "advancedTechnique1" $ do
    it "mimics given example" $ do
      advancedTechnique1 [[O,None,None,O,None,None,X,None,None,O],[X,O,X,O,X,O,X,O,X,O]]
      `shouldBe` [[O,X,None,O,None,None,X,None,None,O],[X,O,X,O,X,O,X,O,X,O]]
    it "does not remove any marks initially on the board" $
      forAll genBoard (\b -> sameMarks b (advancedTechnique1 b))

  describe "avoidTriples3" $ do
    it "places the last of a specific mark on a row such the other marks don't form a triple" $
      avoidingTriples3 [[None, X, O, X, None, O, None, None],
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

    it "does not remove any marks initially on the board" $
      forAll genBoard (\b -> sameMarks b (avoidingTriples3 b))

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

    it "does not remove any marks initially on the board" $
      forAll genBoard (\b -> sameMarks b (completeRow b))

  describe "avoidDuplication" $ do
    it "completes a row if it is nearly identical to another row" $
      avoidDuplication [[X, O, X, None, None, None, O, None],
                        [X, X, O, X, None, None, None, None],
                        [O, O, X, None, None, X, None, None],
                        [O, X, O, None, None, O, X, X],
                        [X, O, X, None, None, O, None, None],
                        [X, X, O, X, O, X, O, O],
                        [O, X, O, O, X, O, X, X],
                        [O, O, X, X, O, X, None, None]]
      `shouldBe`
      [[X, O, X, None, None, None, O, None],
        [X, X, O, X, None, None, None, None],
        [O, O, X, None, None, X, None, None],
        [O, X, O, None, None, O, X, X],
        [X, O, X, None, None, O, None, None],
        [X, X, O, X, O, X, O, O],
        [O, X, O, O, X, O, X, X],
        [O, O, X, X, O, X, None, None]]

