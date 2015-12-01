module SudokuSpec (spec) where
import Control.Exception (evaluate)
import Sudoku
import Test.Hspec


spec :: Spec
spec =
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (24 :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
