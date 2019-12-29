module SolveTest
  ( solveTests
  ) where

import           Board (readBoard)
import           Candidates (createCandidates)
import           Solve
import           TestUtils (isValidSolvedSudoku)
import Control.Monad.Writer (runWriter)
import           Test.HUnit
import           Data.IntSet as IntSet

testSearchSoleCandidates :: Test
testSearchSoleCandidates =
  TestCase
    (assertEqual
      "for createCandidates ..."
      [(5, (5, 5))]
      (searchSoleCandidates candidates))
  where
    candidates = createCandidates [
        (IntSet.fromList [5, 6], (4, 5))
      , (IntSet.fromList [5], (5, 5))]

testSearchUniqueCandidates :: Test
testSearchUniqueCandidates =
  TestCase
    (assertEqual
      "for createCandidates ..."
      [(5, (5, 5))]
      (searchUniqueCandidates candidates))
  where
    candidates = createCandidates [
        (IntSet.fromList [6], (4, 5))
      , (IntSet.fromList [5, 6], (5, 5))
      , (IntSet.fromList [6], (4, 6))
      , (IntSet.fromList [6], (5, 6))]

testSquareRowInteractions :: Test
testSquareRowInteractions =
  TestCase
    (assertEqual
      "for createCandidates ..."
      [(7, (5, 2))]
      (squareRowInteractions candidates))
  where
    candidates = createCandidates [
        (IntSet.fromList [7], (5, 4))
      , (IntSet.fromList [7], (5, 6))
      , (IntSet.fromList [7], (5, 2))
      , (IntSet.fromList [7], (6, 2))]
  
testSquareColumnInteractions :: Test
testSquareColumnInteractions =
  TestCase
    (assertEqual
      "for createCandidates ..."
      [(7, (2, 5))]
      (squareColumnInteractions candidates))
  where
    candidates = createCandidates [
        (IntSet.fromList [7], (4, 5))
      , (IntSet.fromList [7], (6, 5))
      , (IntSet.fromList [7], (2, 5))
      , (IntSet.fromList [7], (2, 6))]

testSquareSquareInteractionsRow :: Test
testSquareSquareInteractionsRow =
  TestCase
    (assertEqual
      "for createCandidates ..."
      [(8, (4, 8))]
      (squareSquareInteractionsRow candidates))
  where
    candidates = createCandidates [
        (IntSet.fromList [8], (4, 1))
      , (IntSet.fromList [8], (4, 2))
      , (IntSet.fromList [8], (6, 1))
      , (IntSet.fromList [8], (6, 2))
      , (IntSet.fromList [8], (4, 5))
      , (IntSet.fromList [8], (4, 6))
      , (IntSet.fromList [8], (6, 5))
      , (IntSet.fromList [8], (6, 6))
      , (IntSet.fromList [8], (4, 8))
      , (IntSet.fromList [8], (5, 8))]

testSquareSquareInteractionsColumn :: Test
testSquareSquareInteractionsColumn =
  TestCase
    (assertEqual
      "for createCandidates ..."
      [(8, (8, 4))]
      (squareSquareInteractionsColumn candidates))
  where
    candidates = createCandidates [
        (IntSet.fromList [8], (1, 4))
      , (IntSet.fromList [8], (2, 4))
      , (IntSet.fromList [8], (1, 6))
      , (IntSet.fromList [8], (2, 6))
      , (IntSet.fromList [8], (5, 4))
      , (IntSet.fromList [8], (6, 4))
      , (IntSet.fromList [8], (5, 6))
      , (IntSet.fromList [8], (6, 6))
      , (IntSet.fromList [8], (8, 4))
      , (IntSet.fromList [8], (8, 5))]

testEliminateCandidatesByNakedSubsets :: Test
testEliminateCandidatesByNakedSubsets =
  TestCase
    (assertEqual
      "for createCandidates ..."
      [(4, (2, 1)), (7, (2, 1)), (7, (4, 1)), (4, (6, 1))]
      (eliminateCandidatesByNakedSubsets candidates))
  where
    candidates = createCandidates [
        (IntSet.fromList [4, 7], (1, 1))
      , (IntSet.fromList [1, 4, 7], (2, 1))
      , (IntSet.fromList [2, 6, 7], (4, 1))
      , (IntSet.fromList [4, 7], (5, 1))
      , (IntSet.fromList [2, 4], (6, 1))]

testEliminateCandidatesByHiddenSubsets :: Test
testEliminateCandidatesByHiddenSubsets =
  TestCase
    (assertEqual
      "for createCandidates ..."
      [(2, (5, 1)), (3, (5, 1)), (5, (5, 1)), (2, (6, 1)), (3, (6, 1)), (5, (6, 1))]
      (eliminateCandidatesByHiddenSubsets candidates))
  where
    candidates = createCandidates [
        (IntSet.fromList [2, 3], (4, 1))
      , (IntSet.fromList [2, 3, 5, 6, 7], (5, 1))
      , (IntSet.fromList [2, 3, 5, 6, 7], (6, 1))
      , (IntSet.fromList [2, 9], (7, 1))
      , (IntSet.fromList [2, 3, 5], (8, 1))
      , (IntSet.fromList [2, 3, 9], (9, 1))]


testSolveEasy :: Test
testSolveEasy =
  TestCase ( do
    initialBoard <- fmap readBoard (readFile "data/board.txt")
    let (finalBoard, logs) =  runWriter (solve initialBoard)
    solution <- fmap readBoard (readFile "data/solution.txt")
    (assertEqual
      "for (solve '/data/board.txt'),"
      solution
      finalBoard))

testSolveHard :: Test
testSolveHard =
  TestCase ( do
    initialBoard <- fmap readBoard (readFile "data/board_hard.txt")
    let (finalBoard, logs) =  runWriter (solve initialBoard)
    (assertEqual
      "for (solve '/data/board_hard.txt'),"
      True
      (isValidSolvedSudoku finalBoard)))

solveTests :: Test
solveTests = TestLabel "SolveTest"
  (TestList [
      TestLabel "testSearchSoleCandidates" testSearchSoleCandidates
    , TestLabel "testSearchUniqueCandidates" testSearchUniqueCandidates
    , TestLabel "testSquareRowInteractions" testSquareRowInteractions
    , TestLabel "testSquareColumnInteractions" testSquareColumnInteractions
    , TestLabel "testSquareSquareInteractionsRow" testSquareSquareInteractionsRow
    , TestLabel "testSquareSquareInteractionsColumn" testSquareSquareInteractionsColumn
    , TestLabel "testEliminateCandidatesByNakedSubsets" testEliminateCandidatesByNakedSubsets
    , TestLabel "testEliminateCandidatesByHiddenSubsets" testEliminateCandidatesByHiddenSubsets
    , TestLabel "testSolveEasy" testSolveEasy
    , TestLabel "testSolveHard" testSolveHard
    ]
  )