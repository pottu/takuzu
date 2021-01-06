module ParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Board
import Parser
import Generators

spec :: Spec
spec = do
  describe "parse" $ do
    it "parses arbitrary correct boards" $
      forAll genBoard (\b -> (parse $ "r c\n" ++ prettyBoard b) `shouldBe` b)
