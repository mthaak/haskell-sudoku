module Solve
  ( solve
  , searchSoleCandidates
  , searchUniqueCandidates
  , squareRowInteractions
  , squareColumnInteractions
  , squareSquareInteractionsRow
  , squareSquareInteractionsColumn
  , eliminateCandidatesByNakedSubsets
  ) where
import           Board
import           Candidates
import           Utils
import           qualified Data.Tuple as Tuple
import           qualified Data.List as List
import           Data.Set (powerSet, intersection)
import           Data.IntSet (IntSet)
import           qualified Data.IntSet as IntSet
import           Data.Matrix (Matrix, matrix, getElem)
import           qualified Data.Matrix as Matrix
import           Debug.Trace (trace, traceShow, traceShowId)
import           Data.Ord (comparing)
import           Control.Monad.Writer
import           Data.List (nub, nubBy, groupBy, sortBy)

-- Tries to solve the board
solve :: Board -> Writer [String] Board
solve board = do
  tell ["Initial board: ", showBoard board]
  tell ["Initial candidates: ", showCandidates candidates]
  solveLoop board candidates 1
  where
    candidates = generateCandidates board

solveLoop :: Board -> Candidates -> Int -> Writer [String] Board
solveLoop board candidates numIterations = do
  tell [replicate 120 '-']
  tell ["Iteration: " ++ show numIterations]
  tell ["Found numbers sole: ", show newNumbersSole]
  tell ["Found numbers unique: ", show newNumbersUnique]
  tell ["Updated board: ", showBoard updatedBoard]
  tell ["Updated candidates: ", showCandidates updatedCandidates]
  if numKnown updatedBoard == 81 -- TODO untangle
    then return updatedBoard
    else if (length newNumbers) >= 1
      then solveLoop updatedBoard updatedCandidates (numIterations + 1)
      else do
        let (removableCandidates, logs) = runWriter (lookForRemovableCandidates candidates)
        tell logs
        if (length removableCandidates) >= 1
          then solveLoop board (removeCandidates removableCandidates candidates) (numIterations + 1)
        else return board
  where
    newNumbersSole = searchSoleCandidates candidates
    newNumbersUnique = searchUniqueCandidates candidates
    newNumbers = newNumbersSole ++ newNumbersUnique
    updatedBoard = updateBoard newNumbers board
    updatedCandidates = updateCandidates newNumbers candidates

-- Looks for cells with only one candidate
searchSoleCandidates :: Candidates -> [(Int, Position)]
searchSoleCandidates candidates = [(intSetHead (getElem i j candidates), (i, j)) |
 (i, j) <- positions, IntSet.size (getElem i j candidates) == 1]
  where
    positions = [(i, j) | i <- [1 .. 9], j <- [1 .. 9]]

-- Looks for cells for which there is a candidate unique in its row, column or square
searchUniqueCandidates :: Candidates -> [(Int, Position)]
searchUniqueCandidates candidates = nub (uniqueCandidates rowsPos
  ++ uniqueCandidates columnsPos
  ++ uniqueCandidates squaresPos)
  where
    uniqueCandidates blocks = concat $ map (lookForUniqueCandidates . joinCandidates candidates) blocks

lookForUniqueCandidates :: [(Int, Position)] -> [(Int, Position)]
lookForUniqueCandidates candidates = uniqueByNum candidates
  where uniqueByNum = filterUniqueEqBy fst

lookForRemovableCandidates :: Candidates -> Writer [String] [(Int, Position)]
lookForRemovableCandidates candidates = do
  tell ["Looking if candidates can be eliminated..."]
  let (msg, removable) = runCandidateEliminators candidateEliminators candidates
  tell [msg]
  return removable

runCandidateEliminators :: [(String, Candidates -> [(Int, Position)])] -> Candidates -> (String, [(Int, Position)])
runCandidateEliminators [] _ = ("Could not eliminate any candidates", [])
runCandidateEliminators ((name, eliminator):other) candidates
  | (length removable) >= 1 = (name ++ ": " ++ (show removable), removable)
  | otherwise = runCandidateEliminators other candidates
 where removable = eliminator candidates

-- TODO rename functions to eliminators?
candidateEliminators :: [(String, Candidates -> [(Int, Position)])]
candidateEliminators = [
    ("squareRowInteractions", squareRowInteractions)
  , ("squareColumnInteractions", squareColumnInteractions)
  , ("squareSquareInteractionsRow", squareSquareInteractionsRow)
  , ("squareSquareInteractionsColumn", squareSquareInteractionsColumn)
  ]

-- TODO make more similar to squareSquareInteractions
-- Looks for candidates that can be eliminated due to square-row interactions
squareRowInteractions :: Candidates -> [(Int, Position)]
squareRowInteractions candidates = filter (candidateIsPresent candidates) $
  concatMap (concatMap toRemovableCandidates . filter inSameRow . groupByNum . joinCandidates candidates) squaresPos
  where
    groupByNum = groupBy (\a b -> (fst a) == (fst b)) . sortBy (comparing fst)
    inSameRow ls = length (nub $ map xRow ls) == 1
    toRemovableCandidates ls =
      let
          number = head (map fst ls)
          row = head (map xRow ls)
          minPos = minimum (map snd ls)
          filterNotInThisSquare = filter (\(num, pos) -> not (inSameSquare minPos pos))
      in filterNotInThisSquare [(number, (row, c)) | c <- [1..9]]

-- Looks for candidates that can be eliminated due to square-column interactions
squareColumnInteractions :: Candidates -> [(Int, Position)]
squareColumnInteractions = swapRowsColumns . squareRowInteractions . Matrix.transpose

-- Looks for candidates that can be eliminated due to square-square interactions (row-wise)
squareSquareInteractionsRow :: Candidates -> [(Int, Position)]
squareSquareInteractionsRow candidates = nub $ concatMap per3squares squares
  where
    squareNums = concatMap List.permutations [[1,2,3], [4,5,6], [7,8,9],
      [1,4,7], [2,5,8], [3,6,9]]
    squares = map (map (\i -> squaresPos !! (i - 1))) squareNums
    per3squares = squareSquareInteractionsFor3Squares . tuplify3 . map (joinCandidates candidates)

squareSquareInteractionsFor3Squares :: ([(Int, Position)], [(Int, Position)], [(Int, Position)]) -> [(Int, Position)]
squareSquareInteractionsFor3Squares (sq1, sq2, sq3) = concatMap f [1..9]
  where
    f num = squareSquareInteractionsForNum num (filterByNum num sq1) (filterByNum num sq2) (filterByNum num sq3)
    filterByNum num = filter ((num ==) . fst)

squareSquareInteractionsForNum :: Int -> [(Int, Position)] -> [(Int, Position)] -> [(Int, Position)] -> [(Int, Position)]
squareSquareInteractionsForNum num sq1 sq2 sq3
  | length (uniqueRows sq1) == 2 &&
    uniqueRows sq1 == uniqueRows sq2 &&
    uniqueRows sq1 /= uniqueRows sq3 = sq3removable
  | otherwise = []
  where
    uniqueRows = nub . map xRow
    sq3removable = filter (\(num, (r, c)) -> List.elem r (uniqueRows sq1)) sq3

-- Looks for candidates that can be eliminated due to square-square interactions (column-wise)
squareSquareInteractionsColumn :: Candidates -> [(Int, Position)]
squareSquareInteractionsColumn = swapRowsColumns . squareSquareInteractionsRow . Matrix.transpose

eliminateCandidatesByNakedSubsets :: Candidates -> [(Int, Position)]
eliminateCandidatesByNakedSubsets candidates = concatMap eliminateCandidatesByNakedSubsetsForBlock blocksWithCandidates
  where
    blocks = rowsPos ++ columnsPos ++ squaresPos
    blocksWithCandidates = map (joinCandidateSets candidates) blocks

eliminateCandidatesByNakedSubsetsForBlock :: [(IntSet, Position)] -> [(Int, Position)]
eliminateCandidatesByNakedSubsetsForBlock block = concatMap (removable block) commonSubsets
  where
    commonSubsets = (map extractSubset . filterGroup . groupBySubset) block
    groupBySubset = groupBy (\a b -> (fst a) == (fst b)) . sortBy (comparing fst)
    filterGroup = filter (\grp -> length grp == IntSet.size (extractSubset grp))
    extractSubset = (fst . head)
    removable block subset = concatMap (\(numSet, pos) ->
        if numSet == subset
          then []
          else flatten [(intSetIntersection numSet subset, pos)]
      ) block
