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

-- 9x9 cells with sets of candidate numbers
type Candidates = Matrix IntSet

-- Tries to solve the board
solve :: Board -> Board
solve board = solveLoop board candidates 100
  where
    candidates = traceShowId (generateCandidates board)

solveLoop :: Board -> Candidates -> Int -> Board
solveLoop board _ 0 = board -- stop at 0 iterationsLeft
solveLoop board candidates iterationsLeft =
  if numKnown newBoard == 81
    then newBoard
    else solveLoop newBoard newCandidates (iterationsLeft - 1)
  where
    newNumbers = solveUniqueCandidate candidates
    newBoard = updateBoard newNumbers board
    newCandidates = updateCandidates newNumbers candidates

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
solveSoleCandidate candidates = soleCandidates
  where
    elements = [(i, j) | i <- [1 .. 9], j <- [1 .. 9]]
    soleCandidates =
      [(intSetHead (getElem i j candidates), (i, j)) | (i, j) <- elements, IntSet.size (getElem i j candidates) == 1]

solveUniqueCandidate :: Candidates -> [(Int, Position)]
solveUniqueCandidate candidates = uniqueCandidates rowsPos
  ++ uniqueCandidates columnsPos
  ++ uniqueCandidates squaresPos
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
removeNeighbourCandidates x pos candidates = foldl remove candidates neighbours
  where
    neighbours = pos : getNeighbours pos -- include the position itself
    remove acc (i, j) = removeCandidate x (i, j) acc

-- Removes candidate Int at Position
removeCandidate :: Int -> Position -> Candidates -> Candidates
removeCandidate candidate pos candidates = mapElemMatrix (IntSet.delete candidate) pos candidates
