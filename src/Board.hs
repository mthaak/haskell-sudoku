module Board
  ( Board
  , Position
  , numKnown
  , updateBoard
  , rowsPos
  , columnsPos
  , squaresPos
  , getNeighbours
  , rowNeighbours
  , columnNeighbours
  , squareNeighbours
  , readBoard
  , showBoard
  ) where

import           Utils (removeLineEndings, boolToInt)
import           Data.Char
import           Data.Matrix as Matrix
import           Data.Vector (Vector)

-- 9x9 matrix of numbers
type Board = Matrix Int

-- Row and column indices (0-8)
type Position = (Int, Int)

-- Number of known numbers on the board
numKnown :: Board -> Int
numKnown = length . filter (/= 0) . Matrix.toList

-- Updates the board with newly found numbers
updateBoard :: [(Int, Position)] -> Board -> Board
updateBoard newNumbers board = foldl f board newNumbers
  where
    f board_ (x, pos) = updateCell x pos board_

-- Sets number at position on the board
updateCell :: Int -> Position -> Board -> Board
updateCell = Matrix.setElem

-- Positions of elements by row
rowsPos :: [[Position]]
rowsPos = [[(i, j) | j <- [1 .. 9]] | i <- [1 .. 9]]

-- Positions of elements by column
columnsPos :: [[Position]]
columnsPos = [[(i, j) | i <- [1 .. 9]] | j <- [1 .. 9]]

-- Positions of elements by squares
squaresPos :: [[Position]]
squaresPos = [[(i, j) | i <- [is .. is + 2], j <- [js .. js + 2]]   | is <- [1, 4, 7], js <- [1, 4, 7]]

-- Neighbours in same row
rowNeighbours :: Position -> [Position]
rowNeighbours (i, j) = [(i, x) | x <- [1 .. 9], x /= j]

-- Neighbours in same column
columnNeighbours :: Position -> [Position]
columnNeighbours (i, j) = [(x, j) | x <- [1 .. 9], x /= i]

-- Neighbours in same square
squareNeighbours :: Position -> [Position]
squareNeighbours (i, j) = [(x, y) | x <- [xs .. xs + 2], y <- [ys .. ys + 2], (x, y) /= (i, j)]
  where
    xs = ((i - 1) `div` 3) * 3 + 1
    ys = ((j - 1) `div` 3) * 3 + 1

-- Row, column and square neighbours
getNeighbours :: Position -> [Position]
getNeighbours pos = rowNeighbours pos ++ columnNeighbours pos ++ squareNeighbours pos

-- Converts lines of numbers to board
readBoard :: String -> Board
readBoard = fmap toInt . Matrix.fromList 9 9 . removeLineEndings

toInt :: Char -> Int
toInt '_' = 0
toInt x   = digitToInt x

toDigit :: Int -> Char
toDigit 0 = '_'
toDigit x = intToDigit x

showBoard :: Board -> String
showBoard = Matrix.prettyMatrix