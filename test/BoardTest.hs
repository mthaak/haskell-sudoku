module BoardTest
  ( boardTests
  ) where

import           Board
import           Test.HUnit

testRowPows :: Test

testRowNeighbours :: Test
testRowNeighbours =
  TestCase
    (assertEqual
       "for (rowNeighbours ((7,6)),"
       [(7, 1), (7, 2), (7, 3), (7, 4), (7, 5), (7, 7), (7, 8), (7, 9)]
       (rowNeighbours (7, 6)))

testColumnNeighbours :: Test
testColumnNeighbours =
  TestCase
    (assertEqual
       "for (columnNeighbours ((7,6)),"
       [(1, 6), (2, 6), (3, 6), (4, 6), (5, 6), (6, 6), (8, 6), (9, 6)]
       (columnNeighbours (7, 6)))

testSquareNeighbours :: Test
testSquareNeighbours =
  TestCase
    (assertEqual
       "for (squareNeighbours ((6,5)),"
       [(4, 4), (4, 5), (4, 6), (5, 4), (5, 5), (5, 6), (6, 4), (6, 6)]
       (squareNeighbours (6, 5)))

boardTests :: Test
boardTests =
  TestList
    [ TestLabel "testRowNeighbours" testRowNeighbours
    , TestLabel "testColumnNeighbours" testColumnNeighbours
    , TestLabel "testSquareNeighbours" testSquareNeighbours
    ]
