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
    it "solves arbitrary solvable boards correct" $
      forAll (fmap solve genSmallBoard `suchThat` isJust)
        (\b -> (check $ fromJust $ b) `shouldBe` Correct)