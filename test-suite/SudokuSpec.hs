module SudokuSpec (spec) where
import Test.Hspec
import Test.QuickCheck
import Sudoku
import Control.Exception (evaluate)

examplePuzzle = unlines [
     "5...8..49"
    , "...5...3."
    , ".673....1"
    , "15......."
    , "...2.8..."
    , ".......18"
    , "7....415."
    , ".3...2..."
    , "49..5...3"]


exampleSolution = unlines [
     "513687249"
    , "849521637"
    , "267349581"
    , "158463972"
    , "974218365"
    , "326795418"
    , "782934156"
    , "635172894"
    , "491856723"]


spec :: Spec
spec =
  describe "singleClauses" $ do
    it "number of single-loc clauses" $ do
        length (singleClauses (0, 0)) `shouldBe` (6^2 + 1)

    it "right number of blocks of proper shape" $ do
        map (\b -> length b == 9) blocks `shouldBe` replicate 27 True

    it "right number of clauses" $ do
        length allClauses `shouldBe` 11745

    it "valuesInLine" $ do
        valuesInLine (0, "1..4..7..") `shouldBe` [((0,0),0),((0,3),3),((0,6),6)]

    it "solveSoduko" $ do
        solution <- solvePuzzle examplePuzzle
        solution `shouldBe` exampleSolution

