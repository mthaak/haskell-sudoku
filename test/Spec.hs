import           Lib
import           Test.HUnit

testGetRowNeighbours :: Test
testGetRowNeighbours =
  TestCase
    (assertEqual
       "for (getRowNeighbours ((7,6)),"
       [(7, 1), (7, 2), (7, 3), (7, 4), (7, 5), (7, 7), (7, 8), (7, 9)]
       (getRowNeighbours (7, 6)))

testGetColumnNeighbours :: Test
testGetColumnNeighbours =
  TestCase
    (assertEqual
       "for (getColumnNeighbours ((7,6)),"
       [(1, 6), (2, 6), (3, 6), (4, 6), (5, 6), (6, 6), (8, 6), (9, 6)]
       (getColumnNeighbours (7, 6)))

testGetSquareNeighbours :: Test
testGetSquareNeighbours =
  TestCase
    (assertEqual
       "for (getSquareNeighbours ((6,5)),"
       [(4, 4), (4, 5), (4, 6), (5, 4), (5, 5), (5, 6), (6, 4), (6, 6)]
       (getSquareNeighbours (6, 5)))

tests :: Test
tests =
  TestList
    [ TestLabel "testGetRowNeighbours" testGetRowNeighbours
    , TestLabel "testGetColumnNeighbours" testGetColumnNeighbours
    , TestLabel "testGetSquareNeighbours" testGetSquareNeighbours
    ]

main :: IO Counts
main = runTestTT tests
