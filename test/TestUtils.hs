module TestUtils
  ( isValidSolvedSudoku
  ) where

import           Board
import           qualified Data.Set as Set
import           Data.Matrix as Matrix

isValidSolvedSudoku :: Board -> Bool
isValidSolvedSudoku board = all allNumbersPresent blocks
  where
    blocks = rowsPos ++ columnsPos ++ squaresPos
    allNumbersPresent block = Set.fromList (numbersIn block) == Set.fromList [1..9]
    numbersIn block = (map (\(r, c) -> Matrix.getElem r c board) block)
