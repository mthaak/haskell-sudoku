module Solve
  ( solve
  , searchSoleCandidates
  , searchUniqueCandidates
  , squareRowInteractions
  , squareColumnInteractions
  , squareSquareInteractionsRow
  , squareSquareInteractionsColumn
  , nakedSubsets
  , hiddenSubsets
  ) where
import           Board
import           Candidates
import           Utils
import           qualified Data.Set as Set
import           qualified Data.Tuple as Tuple
import           qualified Data.List as List
import           Data.IntSet (IntSet)
import           qualified Data.IntSet as IntSet
import           Data.Matrix (Matrix, matrix, getElem)
import           qualified Data.Matrix as Matrix
import           Debug.Trace (trace, traceShow, traceShowId)
import           Data.Ord (comparing)
import           Control.Monad.Writer
import           Data.List (nub, nubBy, groupBy, sortBy, maximumBy)

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

-- Runs the eliminators in sequence until one function returns at least one candidate that can be removed
runCandidateEliminators :: [(String, Candidates -> [(Int, Position)])] -> Candidates -> (String, [(Int, Position)])
runCandidateEliminators [] _ = ("Could not eliminate any candidates", [])
runCandidateEliminators ((name, eliminator):other) candidates
  | (length removable) >= 1 = (name ++ ": " ++ (show removable), removable)
  | otherwise = runCandidateEliminators other candidates
 where removable = eliminator candidates

-- Functions and their names for eliminating candidates
candidateEliminators :: [(String, Candidates -> [(Int, Position)])]
candidateEliminators = [
      ("squareRowInteractions", squareRowInteractions)
    , ("squareColumnInteractions", squareColumnInteractions)
    , ("squareSquareInteractionsRow", squareSquareInteractionsRow)
    , ("squareSquareInteractionsColumn", squareSquareInteractionsColumn)
    , ("nakedSubsets", nakedSubsets)
    , ("hiddenSubsets", hiddenSubsets)
  ]

-- Invokes function over all sets of 3 squares (aligned horizontally or vertically) and all 9 possible numbers
-- and concatenates the results
concatMapOver3SquaresAndNumbers :: (([(Int, Position)], [(Int, Position)], [(Int, Position)]) -> Int -> [(Int, Position)])
  -> Candidates -> [(Int, Position)]
concatMapOver3SquaresAndNumbers per3squaresAndNum candidates = nub iterateSquares
  where
    squareNumbers = concatMap List.permutations [[1,2,3], [4,5,6], [7,8,9], [1,4,7], [2,5,8], [3,6,9]]
    squares = map (map (\i -> squaresPos !! (i - 1))) squareNumbers
    squaresWithCandidates = map (map (joinCandidates candidates)) squares
    iterateSquares = concatMap iterateNumbers squaresWithCandidates
    iterateNumbers [sq1,sq2,sq3] = concatMap (\num -> per3squaresAndNum (filterByNum num sq1,
                                                                         filterByNum num sq2,
                                                                         filterByNum num sq3) num) [1..9]
    filterByNum num = filter ((num ==) . fst)

-- ############# ELIMINATE CANDIDATES BY SQUARE-ROW/COL INTERACTIONS ##########

-- Looks for candidates that can be eliminated due to square-row interactions
squareRowInteractions :: Candidates -> [(Int, Position)]
squareRowInteractions candidates = concatMapOver3SquaresAndNumbers squareRowInteraction candidates

-- A single square-row interaction for 3 aligned squares and one number
squareRowInteraction :: ([(Int, Position)], [(Int, Position)], [(Int, Position)]) -> Int -> [(Int, Position)]
squareRowInteraction (sq1, sq2, sq3) num
  | length uniqueRowsSq1 == 1 = removable
  | otherwise = []
  where
    uniqueRowsSq1 = (nub . map xRow) sq1
    removable = filter (\(num, (r, c)) -> List.elem r uniqueRowsSq1) (sq2 ++ sq3)

-- Looks for candidates that can be eliminated due to square-column interactions
squareColumnInteractions :: Candidates -> [(Int, Position)]
squareColumnInteractions = swapRowsColumns . squareRowInteractions . Matrix.transpose

-- ############# ELIMINATE CANDIDATES BY SQUARE-SQUARE INTERACTIONS ###########

-- Looks for candidates that can be eliminated due to square-square interactions (row-wise)
squareSquareInteractionsRow :: Candidates -> [(Int, Position)]
squareSquareInteractionsRow candidates = concatMapOver3SquaresAndNumbers squareSquareInteraction candidates

-- A single square-square interaction (row-wise) for 3 aligned squares and one number
squareSquareInteraction :: ([(Int, Position)], [(Int, Position)], [(Int, Position)]) -> Int -> [(Int, Position)]
squareSquareInteraction (sq1, sq2, sq3) num
  | length (uniqueRows sq1) == 2 &&
    uniqueRows sq1 == uniqueRows sq2 &&
    uniqueRows sq1 /= uniqueRows sq3 = removable
  | otherwise = []
  where
    uniqueRows = nub . map xRow
    removable = filter (\(num, (r, c)) -> List.elem r (uniqueRows sq1)) sq3

-- Looks for candidates that can be eliminated due to square-square interactions (column-wise)
squareSquareInteractionsColumn :: Candidates -> [(Int, Position)]
squareSquareInteractionsColumn = swapRowsColumns . squareSquareInteractionsRow . Matrix.transpose

-- #################### ELIMINATE CANDIDATES BY NAKED SUBSETS #################

-- Looks for candidates that can be eliminated due to naked subsets
nakedSubsets :: Candidates -> [(Int, Position)]
nakedSubsets candidates = concatMap nakedSubsetsForBlock blocksWithCandidates
  where
    blocks = rowsPos ++ columnsPos ++ squaresPos
    blocksWithCandidates = map (joinCandidateSets candidates) blocks

nakedSubsetsForBlock :: [(IntSet, Position)] -> [(Int, Position)]
nakedSubsetsForBlock block = concatMap removable commonSubsets
  where
    commonSubsets = (map extractSubset . filterGroup . groupBySubset) block
    groupBySubset = groupBy (\a b -> (fst a) == (fst b)) . sortBy (comparing fst)
    filterGroup = filter (\grp -> length grp == IntSet.size (extractSubset grp))
    extractSubset = (fst . head)
    removable subset = concatMap (\(numSet, pos) ->
        if numSet == subset
          then []
          else flatten [(IntSet.intersection numSet subset, pos)]
      ) block

-- #################### ELIMINATE CANDIDATES BY HIDDEN SUBSETS ################

-- Looks for candidates that can be eliminated due to hidden subsets
hiddenSubsets :: Candidates -> [(Int, Position)]
hiddenSubsets candidates = nub $ concatMap f blocks
  where
    blocks = rowsPos ++ columnsPos ++ squaresPos
    f block = concatMap (uncurry (g block)) (splitsMinSize 2 (Set.fromList [1..9]))
    g block a b = h (toPos block (Set.toList a)) (toPos block (Set.toList b))
    toPos block indices = map (\i -> block !! (i - 1)) indices
    h a b = hiddenSubsetsForInclExcl (getCandidates candidates a)
                                                          (getCandidates candidates b)

hiddenSubsetsForInclExcl :: [(IntSet, Position)] -> [(IntSet, Position)] -> [(Int, Position)]
hiddenSubsetsForInclExcl cellsIncl cellsExcl
  | (length uniqueSubsets) > 0 = removable
  | otherwise = []
  where
    largestSubset = foldl1 IntSet.intersection (map xNumSet cellsIncl)
    subsets = subsetsOfSize (length cellsIncl) largestSubset
    isUnique subset = all (\(numSet, pos) -> IntSet.intersection subset numSet == IntSet.empty) cellsExcl
    uniqueSubsets = filter isUnique subsets
    largestUniqueSubset = maximumBy (comparing IntSet.size) uniqueSubsets
    removable = flatten $ map (\(numSet, pos) -> (IntSet.difference numSet largestUniqueSubset, pos)) cellsIncl
