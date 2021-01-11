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
