module Board
  ( Board
  , Position
  , numKnown
  , knownPos
  , updateBoard
  , rowsPos
  , columnsPos
  , squaresPos
  , inSameSquare
  , getNeighbours
  , rowNeighbours
  , columnNeighbours
  , squareNeighbours
  , readBoard
  , showBoard
  ) where

import           Utils (removeLineEndings, boolToInt)
import           Data.Char
import           Data.Matrix (Matrix)
import           qualified Data.Matrix as Matrix

-- 9x9 matrix of numbers
type Board = Matrix Int

-- Row and column indices (0-8)
type Position = (Int, Int)

-- Number of known numbers on the board
numKnown :: Board -> Int
numKnown = length . filter (/= 0) . Matrix.toList

-- Returns the known numbers with their positions
knownPos :: Board -> [(Int, Position)]
knownPos = filter ((0 /=) . fst) . Matrix.toList . Matrix.mapPos (\(r,c) x -> (x, (r, c)))

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
rowsPos = [[(r, c) | c <- [1 .. 9]] | r <- [1 .. 9]]

-- Positions of elements by column
columnsPos :: [[Position]]
columnsPos = [[(r, c) | r <- [1 .. 9]] | c <- [1 .. 9]]

-- Positions of elements by squares
squaresPos :: [[Position]]
squaresPos = [[(r, c) | r <- [rs .. rs + 2], c <- [cs .. cs + 2]] | rs <- [1, 4, 7], cs <- [1, 4, 7]]

-- Checks whether the given positions are in the same square
inSameSquare :: Position -> Position -> Bool
inSameSquare (r1, c1) (r2, c2) = floor3 r1 == floor3 r2 && floor3 c1 == floor3 c2
  where floor3 n = ((n - 1) `div` 3) * 3 + 1

-- Neighbours in same row
rowNeighbours :: Position -> [Position]
rowNeighbours (i, j) = [(i, c) | c <- [1 .. 9], c /= j]

-- Neighbours in same column
columnNeighbours :: Position -> [Position]
columnNeighbours (i, j) = [(r, j) | r <- [1 .. 9], r /= i]

-- Neighbours in same square
squareNeighbours :: Position -> [Position]
squareNeighbours (i, j) = [(r, c) | r <- [rs .. rs + 2], c <- [cs .. cs + 2], (r, c) /= (i, j)]
  where
    rs = ((i - 1) `div` 3) * 3 + 1
    cs = ((j - 1) `div` 3) * 3 + 1

-- Row, column and square neighbours
getNeighbours :: Position -> [Position]
getNeighbours pos = rowNeighbours pos ++ columnNeighbours pos ++ squareNeighbours pos

-- Converts lines of numbers to board
readBoard :: String -> Board
readBoard = fmap toInt . Matrix.fromList 9 9 . removeLineEndings

toInt :: Char -> Int
toInt '.' = 0
toInt x   = digitToInt x

toDigit :: Int -> Char
toDigit 0 = '.'
toDigit x = intToDigit x

showBoard :: Board -> String
showBoard = Matrix.prettyMatrix