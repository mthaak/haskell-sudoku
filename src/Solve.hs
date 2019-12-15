module Solve
  ( solve
  ) where
import           Board
import           Utils
import           Data.IntSet (IntSet)
import           Data.IntSet as IntSet hiding (filter, foldl, map)
import           Data.Matrix (Matrix, matrix)
import           Data.Matrix as Matrix hiding (flatten, trace)
import           Debug.Trace (trace, traceShow, traceShowId)
import           Control.Monad.Writer
import           Data.List (nub, nubBy)

-- 9x9 cells with sets of candidate numbers
type Candidates = Matrix IntSet

-- Tries to solve the board
solve :: Board -> Writer [String] Board
solve board = do
  tell ["Initial board: ", showBoard board]
  tell ["Initial candidates: ", showCandidates candidates]
  solveLoop board candidates 100
  where
    candidates = generateCandidates board

solveLoop :: Board -> Candidates -> Int -> Writer [String] Board
solveLoop board _ 0 = return board -- stop at 0 iterationsLeft
solveLoop board candidates iterationsLeft = do
  tell [replicate 120 '-']
  tell ["Iterations left: " ++ show iterationsLeft]
  tell ["Found numbers sole: ", show newNumbersSole]
  tell ["Found numbers unique: ", show newNumbersUnique]
  tell ["Updated board: ", showBoard updatedBoard]
  tell ["Updated candidates: ", showCandidates candidates]
  if (numKnown updatedBoard == 81) || (newNumbers == [])
    then return updatedBoard
    else solveLoop updatedBoard updatedCandidates (iterationsLeft - 1)
  where
    newNumbersSole = solveSoleCandidate candidates
    newNumbersUnique = solveUniqueCandidate candidates
    newNumbers = newNumbersSole ++ newNumbersUnique
    updatedBoard = updateBoard newNumbers board
    updatedCandidates = updateCandidates newNumbers candidates

-- Generates the candidates from the given board
generateCandidates :: Board -> Candidates
generateCandidates board = foldl h initialCandidates present
  where
    initialCandidates = matrix 9 9 $ \_ -> IntSet.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9] -- all candidates
    present = [(i, j) | i <- [1 .. 9], j <- [1 .. 9], getElem i j board /= 0]
    f acc pos = setElem IntSet.empty pos acc
    g acc (i, j) = removeNeighbourCandidates (getElem i j board) (i, j) acc
    h acc pos = g (f acc pos) pos

-- Looks for cells with only one candidate
solveSoleCandidate :: Candidates -> [(Int, Position)]
solveSoleCandidate candidates = [(intSetHead (getElem i j candidates), (i, j)) |
 (i, j) <- positions, IntSet.size (getElem i j candidates) == 1]
  where
    positions = [(i, j) | i <- [1 .. 9], j <- [1 .. 9]]

solveUniqueCandidate :: Candidates -> [(Int, Position)]
solveUniqueCandidate candidates = nub (uniqueCandidates rowsPos
  ++ uniqueCandidates columnsPos
  ++ uniqueCandidates squaresPos)
  where
    uniqueCandidates blocks = concat $ map (lookForUniqueCandidates . withCandidates) blocks
    withCandidates block = [(getElem i j candidates, (i, j)) | (i, j) <- block]

lookForUniqueCandidates :: [(IntSet, Position)] -> [(Int, Position)]
lookForUniqueCandidates candidates = (uniqueByNum . flatten) candidates
  where uniqueByNum = filterUniqueEqBy fst

flatten :: [(IntSet, Position)] -> [(Int, Position)]
flatten list = concat (map flat list)
  where flat (set, pos) = map (\n -> (n, pos)) (elems set)

-- Updates the candidates with newly found numbers
updateCandidates :: [(Int, Position)] -> Candidates -> Candidates
updateCandidates newNumbers candidates = foldl f candidates newNumbers
  where
    f candidates_ (x, pos) = removeNeighbourCandidates x pos candidates_

-- Removes all alternate candidates of Int around Pos
removeNeighbourCandidates :: Int -> Position -> Candidates -> Candidates
removeNeighbourCandidates x pos candidates = removeAllCandidates pos neighbourCandidatesRemoved
  where
    neighbourCandidatesRemoved = foldl (\acc pos -> removeCandidate x pos acc) candidates (getNeighbours pos)

-- Removes candidate Int at Position
removeCandidate :: Int -> Position -> Candidates -> Candidates
removeCandidate candidate pos candidates = mapElemMatrix (IntSet.delete candidate) pos candidates

-- Removes all candidates at Position
removeAllCandidates :: Position -> Candidates -> Candidates
removeAllCandidates pos candidates = Matrix.setElem IntSet.empty pos candidates

-- Shows candidates as a matrix
showCandidates :: Candidates -> String
showCandidates candidates = show (fmap IntSet.toList candidates)