module Candidates
  ( Candidates
  , generateCandidates
  , updateCandidates
  , swapRowsColumns
  , joinCandidates
  , joinCandidateSets
  , removeCandidates
  , createCandidates
  , candidateIsPresent
  , showCandidates
  , xNum
  , xRow
  , xCol
  ) where
import           Board
import           Utils
import           Data.Tuple as Tuple
import           Data.List as List
import           Data.IntSet (IntSet)
import           Data.IntSet as IntSet hiding (filter, foldl, map)
import           Data.Matrix (Matrix, matrix)
import           Data.Matrix as Matrix hiding (flatten, trace)
import           Debug.Trace (trace, traceShow, traceShowId)
import           Data.Ord (comparing)
import           Data.List (nub, nubBy, groupBy, sortBy)

-- 9x9 cells with sets of candidate numbers
type Candidates = Matrix IntSet

-- Generates the candidates from the given board
generateCandidates :: Board -> Candidates
generateCandidates board = updateCandidates (knownPos board) initialCandidates
  where
    initialCandidates = matrix 9 9 $ \_ -> IntSet.fromList [1 .. 9] -- start from all candidates

swapRowsColumns :: [(a, Position)] -> [(a, Position)]
swapRowsColumns = map (\(a, pos) -> (a, Tuple.swap pos))

-- Add the individual candidates to the given positions
joinCandidates :: Candidates -> [Position] -> [(Int, Position)]
joinCandidates candidates positions = flatten $ joinCandidateSets candidates positions

-- Add the candidate set to each given position
joinCandidateSets :: Candidates -> [Position] -> [(IntSet, Position)]
joinCandidateSets candidates positions = [(getElem i j candidates, (i, j)) | (i, j) <- positions]

flatten :: [(IntSet, Position)] -> [(Int, Position)]
flatten list = concat (map flat list)
  where flat (set, pos) = map (\n -> (n, pos)) (elems set)

-- Updates the candidates with newly found numbers
updateCandidates :: [(Int, Position)] -> Candidates -> Candidates
updateCandidates newNumbers candidates = foldl f candidates newNumbers
  where f candidates (num, pos) = updateCandidatesSingle num pos candidates

-- Updates the candidates with a single newly found number
updateCandidatesSingle :: Int -> Position -> Candidates -> Candidates
updateCandidatesSingle num pos = removeNeighbourCandidates num pos . removeAllCandidates pos

-- Removes all neighbouring candidate positions for number
removeNeighbourCandidates :: Int -> Position -> Candidates -> Candidates
removeNeighbourCandidates num pos candidates = foldl f candidates (getNeighbours pos)
  where f candidates pos = removeCandidate num pos candidates

-- Creates candidates, starting from none
createCandidates :: [(IntSet, Position)] -> Candidates
createCandidates toSet = foldl f emptyCandidates toSet
  where
    emptyCandidates = matrix 9 9 $ \_ -> IntSet.empty
    f candidates (set, pos) = setCandidatesAtPos set pos candidates

-- Sets the candidate set at position
setCandidatesAtPos :: IntSet -> Position -> Candidates -> Candidates
setCandidatesAtPos candidateSet pos candidates = Matrix.setElem candidateSet pos candidates

-- Adds multiple candidates
addCandidates :: [(Int, Position)] -> Candidates  -> Candidates
addCandidates toAdd candidates = foldl f candidates toAdd
  where f candidates (num, pos) = addCandidate num pos candidates

-- Adds candidate to position
addCandidate :: Int -> Position -> Candidates -> Candidates
addCandidate number pos candidates = mapElemMatrix (IntSet.insert number) pos candidates

-- Removes multiple candidates
removeCandidates :: [(Int, Position)] -> Candidates  -> Candidates
removeCandidates toRemove candidates = foldl f candidates toRemove
  where f candidates (num, pos) = removeCandidate num pos candidates

-- Removes candidate from position
removeCandidate :: Int -> Position -> Candidates -> Candidates
removeCandidate number pos candidates = mapElemMatrix (IntSet.delete number) pos candidates

-- Removes all candidates from position
removeAllCandidates :: Position -> Candidates -> Candidates
removeAllCandidates pos = setCandidatesAtPos IntSet.empty pos

-- Looks whether given candidate is present
candidateIsPresent :: Candidates -> (Int, Position) -> Bool
candidateIsPresent candidates (num, (r, c)) = IntSet.member num (getElem r c candidates)

-- Shows candidates as a matrix
showCandidates :: Candidates -> String
showCandidates candidates = show (fmap IntSet.toList candidates)

-- Extract number
xNum :: (Int, Position) -> Int
xNum = fst

-- Extract row
xRow :: (a, Position) -> Int
xRow (a, (r, c)) = r

-- Extract column
xCol :: (a, Position) -> Int
xCol (a, (r, c)) = c

