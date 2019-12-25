module SolveTest
  ( solveTests
  ) where

import           Board (readBoard)
import           Solve (solve)
import           TestUtils (isValidSolvedSudoku)
import Control.Monad.Writer (runWriter)
import           Test.HUnit


testSolve :: Test
testSolve =
  TestCase ( do
    initialBoard <- fmap readBoard (readFile "data/board.txt")
    let (finalBoard, logs) =  runWriter (solve initialBoard)
    solution <- fmap readBoard (readFile "data/solution.txt")
    (assertEqual
      "for (solve '/data/board.txt'),"
      solution
      finalBoard))

testSolve2 :: Test
testSolve2 =
  TestCase ( do
    initialBoard <- fmap readBoard (readFile "data/board_hard.txt")
    let (finalBoard, logs) =  runWriter (solve initialBoard)
    (assertEqual
      "for (solve '/data/board_hard.txt'),"
      True
      (isValidSolvedSudoku finalBoard)))

solveTests :: Test
solveTests = TestLabel "SolveTest"
  (TestList
    [ TestLabel "testSolve" testSolve
    , TestLabel "testSolve2" testSolve2
    ]
  )