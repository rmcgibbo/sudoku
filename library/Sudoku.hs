{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | TODO
module Sudoku (module Sudoku) where

import Data.Char (intToDigit)
import Data.List (elemIndices, intersperse, sort, transpose)
import Data.List.Split (chunksOf)
import Data.Tuple (uncurry)
import Control.Applicative (pure)
import Picosat

----------------------------------------
-- Building the boolean constraints
---------------------------------------

type Var = Int
type Clause = [Var]
type Loc = (Int, Int)

-- | Boolean variable for the cell at `loc` to take the value `d`.
var :: Loc -> Int -> Var
var (i, j) d =
    81*i + 9*j + d + 1

prod :: [a] -> [(a, a)]
prod as = [(x,y) | x <- as, y <- as]

pairs :: Ord a => [a] -> [(a, a)]
pairs as = [(x,y) | x <- as, y <- as, x < y]


-- | Logical clauses that constrain each single cell to take only 1 value.
singleClauses :: Loc -> [Clause]
singleClauses loc = atLeastOne:lessThanTwo
    where atLeastOne =  map (var loc) [0..8]
          lessThanTwo = [[-var loc d1, -var loc d2] | (d1, d2) <- pairs [0..8]]

-- | Logical clauses that constrain a list of locations to take distinct values
blockClauses :: [Loc] -> [Clause]
blockClauses locs = concatMap pairClauses (pairs locs)
    where pairClauses (i, j) = [[-var i d, -var j d] | d <- [0..8]]

-- | A list of the rows, columns, and 3x3 quadrants
blocks :: [[Loc]]
blocks = rows ++ columns ++ threeByThrees
    where columns = [[(i,j) | i <- [0..8]] | j <- [0..8]]
          rows = transpose columns
          create3x3Block (i, j) = [(x,y) | x <- [i..i+2], y <- [j..j+2]]
          threeByThrees = map create3x3Block (prod [0,3..8])

-- |  All of the basic constraints that are common to every sudoku puzzle
allClauses :: [Clause]
allClauses = concatMap singleClauses (prod [0..8]) ++ concatMap blockClauses blocks


-----------------------------------
-- Read the puzzle from a file
-----------------------------------

-- | All of the values on the `i`th line of the file.
valuesInLine :: (Int, String) -> [(Loc, Int)]
valuesInLine (i, line) = concatMap findD [1..9]
    where indexOfChar c = elemIndices c line
          findD d = map (\j -> ((i, j), d-1)) (indexOfChar (intToDigit d))

-- | Find all of the values in given in the puzzle.
loadPuzzle :: String -> [(Loc, Int)]
loadPuzzle puzzle = concatMap valuesInLine lns
    where lns = zip [0..] (lines puzzle)

-- | Extra clauses that enforce all of the cells given in the puzzle
constraintClauses :: String -> [Clause]
constraintClauses puzzle = map (pure . uncurry var) (loadPuzzle puzzle)

-- | Format the solution vector back as a sudoku grid
showSolution :: [Var] -> String
showSolution sol = unlines $ chunksOf 9 $ map digit onvars
    where digit v = intToDigit $ (9:[1..8]) !! (v `mod` 9)
          onvars = filter (> 0) sol

-- | Solve a puzzle and return the filled-in solution.
solvePuzzle  :: String -> IO String
solvePuzzle puzzle = do
    let cnf = allClauses ++ constraintClauses puzzle
    sol <- solve cnf
    case sol of
        Solution s -> return $ showSolution s
        _          -> return $ "Not solvable."


main :: IO ()
main = do
    puzzle <- readFile "puzzle.txt"
    solved <- solvePuzzle puzzle
    putStr solved
