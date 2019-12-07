module Lib
  ( solveSudoku
  , getRowNeighbours
  , getColumnNeighbours
  , getSquareNeighbours
  ) where

import           Data.Char
import           Data.IntSet (IntSet)
import           Data.IntSet as IntSet hiding (filter, foldl)
import           Data.Matrix (Matrix, matrix)
import           Data.Matrix as Matrix

--import           Data.Vector (Vector)
solveSudoku :: IO ()
solveSudoku = fmap (prettyMatrix . solve . strToBoard) readBoard >>= putStrLn

numKnown :: Board -> Int
numKnown board = length [(i, j) | i <- [1 .. 9], j <- [1 .. 9], getElem i j board /= 0]

solve :: Board -> Board
solve board = solveLoop board candidates 100
  where
    candidates = generateCandidates board

solveLoop :: Board -> Candidates -> Int -> Board
solveLoop board _ 0 = board -- stop at 0 iterationsLeft
solveLoop board candidates iterationsLeft =
  if numKnown newBoard == 81
    then newBoard
    else solveLoop newBoard newCandidates (iterationsLeft - 1)
  where
    newNumbers = solveSoleCandidate board candidates
    newBoard = updateBoard newNumbers board
    newCandidates = updateCandidates newNumbers candidates

solveSoleCandidate :: Board -> Candidates -> [(Int, Position)]
solveSoleCandidate board candidates = soleCandidates
  where
    missing = [(i, j) | i <- [1 .. 9], j <- [1 .. 9], getElem i j board == 0]
    soleCandidates =
      [(intSetHead (getElem i j candidates), (i, j)) | (i, j) <- missing, IntSet.size (getElem i j candidates) == 1]

intSetHead :: IntSet -> Int
intSetHead = head . IntSet.elems

-- Updates the board with newly found numbers
updateBoard :: [(Int, Position)] -> Board -> Board
updateBoard newNumbers board = foldl f board newNumbers
  where
    f board_ (x, pos) = updateBoardCell x pos board_

-- Updates the candidates with newly found numbers
updateCandidates :: [(Int, Position)] -> Candidates -> Candidates
updateCandidates newNumbers candidates = foldl f candidates newNumbers
  where
    f candidates_ (x, pos) = removeNeighbourCandidates x pos candidates_

updateBoardCell :: Int -> Position -> Board -> Board
updateBoardCell = setElem

toCell :: Char -> Int
toCell '_' = 0
toCell x   = digitToInt x

type Board = Matrix Int

type Candidates = Matrix IntSet.IntSet

--type Positions = Vector (Vector Int)
type Position = (Int, Int)

-- Removes candidate Int at Position
removeCandidate :: Int -> Position -> Candidates -> Candidates
removeCandidate candidate (i, j) candidates = setElem new (i, j) candidates
  where
    new = IntSet.delete candidate old -- remove candidate from old
    old = getElem i j candidates

-- Generates the candidates from the given board
generateCandidates :: Board -> Candidates
generateCandidates board = foldl h initialCandidates present
  where
    initialCandidates = matrix 9 9 $ \_ -> IntSet.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9] -- all candidates
    present = [(i, j) | i <- [1 .. 9], j <- [1 .. 9], getElem i j board /= 0]
    f acc pos = setElem IntSet.empty pos acc
    g acc (i, j) = removeNeighbourCandidates (getElem i j board) (i, j) acc
    h acc pos = g (f acc pos) pos

-- Removes all alternate candidates of Int around Pos
removeNeighbourCandidates :: Int -> Position -> Candidates -> Candidates
removeNeighbourCandidates x pos candidates = foldl remove candidates neighbours
  where
    neighbours = pos : getNeighbours pos -- include the position itself
    remove acc (i, j) = removeCandidate x (i, j) acc

getRowNeighbours :: Position -> [Position]
getRowNeighbours (i, j) = [(i, x) | x <- [1 .. 9], x /= j]

getColumnNeighbours :: Position -> [Position]
getColumnNeighbours (i, j) = [(x, j) | x <- [1 .. 9], x /= i]

getSquareNeighbours :: Position -> [Position]
getSquareNeighbours (i, j) = [(x, y) | x <- [xStart .. xStart + 2], y <- [yStart .. yStart + 2], (x, y) /= (i, j)]
  where
    xStart = ((i - 1) `div` 3) * 3 + 1
    yStart = ((j - 1) `div` 3) * 3 + 1

getNeighbours :: Position -> [Position]
getNeighbours pos = getRowNeighbours pos ++ getColumnNeighbours pos ++ getSquareNeighbours pos

strToBoard :: String -> Board
strToBoard str = fmap toCell (Matrix.fromList 9 9 strClean)
  where
    strClean = removeLineEndings str

removeLineEndings :: String -> String
removeLineEndings = filter (/= '\n')

readBoard :: IO String
readBoard = readFile "data/board.txt"
