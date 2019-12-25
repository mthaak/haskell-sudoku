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
import           Data.Ord (comparing)
import           Control.Monad.Writer
import           Data.List (nub, nubBy, groupBy, sortBy)

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
  tell ["Updated candidates: ", showCandidates updatedCandidates]
  if numKnown updatedBoard == 81 -- TODO untangle
    then return updatedBoard
    else if (length newNumbers) >= 1
      then solveLoop updatedBoard updatedCandidates (iterationsLeft - 1)
      else do
        let (removableCandidates, logs) = runWriter (lookForRemovableCandidates candidates)
        tell logs
        if (length removableCandidates) >= 1
          then solveLoop board (removeCandidates candidates removableCandidates) (iterationsLeft - 1)
        else return board
  where
    newNumbersSole = solveSoleCandidate candidates
    newNumbersUnique = solveUniqueCandidate candidates
    newNumbers = newNumbersSole ++ newNumbersUnique
    updatedBoard = updateBoard newNumbers board
    updatedCandidates = updateCandidates candidates newNumbers

-- Generates the candidates from the given board
generateCandidates :: Board -> Candidates
generateCandidates board = updateCandidates initialCandidates (knownPos board)
  where
    initialCandidates = matrix 9 9 $ \_ -> IntSet.fromList [1 .. 9] -- start from all candidates

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
    withCandidates = flatten . joinCandidateSets candidates

lookForUniqueCandidates :: [(Int, Position)] -> [(Int, Position)]
lookForUniqueCandidates candidates = uniqueByNum candidates
  where uniqueByNum = filterUniqueEqBy fst

lookForRemovableCandidates :: Candidates -> Writer [String] [(Int, Position)]
lookForRemovableCandidates candidates = do
  tell ["Looking for removable candidates..."]
  let (msg, removable) = runCandidateRemovers candidateRemovers candidates
  tell [msg]
  return removable

runCandidateRemovers :: [(String, Candidates -> [(Int, Position)])] -> Candidates -> (String, [(Int, Position)])
runCandidateRemovers [] _ = ("Could not find removable candidates", [])
runCandidateRemovers ((name, remover):otherRemovers) candidates
  | (length removable) >= 1 = (name ++ ": " ++ (show removable), removable)
  | otherwise = runCandidateRemovers otherRemovers candidates
 where removable = remover candidates

candidateRemovers :: [(String, Candidates -> [(Int, Position)])]
candidateRemovers = [
  ("squareRowInteractions", squareRowInteractions),
  ("squareColumnInteractions", squareColumnInteractions),
  ("squareSquareInteractionsRow", squareSquareInteractionsRow),
  ("squareSquareInteractionsColumn", squareSquareInteractionsColumn)
  ]

-- Returns the candidates that can be removed due to square-row interactions
squareRowInteractions :: Candidates -> [(Int, Position)]
squareRowInteractions candidates = filter (candidateIsPresent candidates) $
  concatMap (concatMap toRemovableCandidates . filter inSameRow . groupByNum . joinCandidates candidates) squaresPos
  where
    groupByNum = groupBy (\a b -> (fst a) == (fst b)) . sortBy (comparing fst)
    inSameRow ls = length (nub $ map (\(num, (r, c)) -> r) ls) == 1
    toRemovableCandidates ls =
      let
          number = head (map fst ls)
          row = head (map (\(num, (r, c)) -> r) ls)
          minColumn = minimum (map (\(num, (r, c)) -> c) ls)
          blockColumn = ((minColumn - 1) `div` 3) * 3 + 1
      in [(number, (row, c)) | c <- [1..9], c < blockColumn || c > blockColumn + 2]

-- Returns the candidates that can be removed due to square-column interactions
squareColumnInteractions :: Candidates -> [(Int, Position)]
squareColumnInteractions candidates = filter (candidateIsPresent candidates) $
  concatMap (concatMap toRemovableCandidates . filter inSameColumn . groupByNum . joinCandidates candidates) squaresPos
  where
    groupByNum = groupBy (\a b -> (fst a) == (fst b)) . sortBy (comparing fst)
    inSameColumn ls = length (nub $ map (\(num, (r, c)) -> c) ls) == 1
    toRemovableCandidates ls =
      let
          number = head (map fst ls)
          column = head (map (\(num, (r, c)) -> c) ls)
          minRow = minimum (map (\(num, (r, c)) -> r) ls)
          blockRow = ((minRow- 1) `div` 3) * 3 + 1
      in [(number, (r, column)) | r <- [1..9], r < blockRow || r > blockRow + 2]

-- TODO generalize to include squareRowInteractions
squareSquareInteractionsRow :: Candidates -> [(Int, Position)]
squareSquareInteractionsRow candidates = filter (candidateIsPresent candidates) $
  concatMap (concatMap toRemovableCandidates . filter coverTwoRows . groupByNum . joinCandidates candidates) squares
  where
    squareNums = [(1,2), (1,3), (2,3), (4,5), (4,6), (5,6), (7,8), (7,9), (8,9),
                  (1,4), (1,7), (4,7), (2,5), (2,8), (5,8), (3,6), (3,9), (6,9)]
    squares = map (\(i,j) -> (squaresPos !! (i-1) ++ squaresPos !! (j-1))) squareNums
    groupByNum = groupBy (\a b -> (fst a) == (fst b)) . sortBy (comparing fst)
    minPos ls = minimum (map snd ls)
    maxPos ls = maximum (map snd ls)
    coverTwoRows ls = length (nub $ map (\(num, (r, c)) -> r) ls) == 2 && (not (inSameSquare (minPos ls) (maxPos ls)))
    toRemovableCandidates ls =
      let
          number = head (map fst ls)
          rows = nub (map (\(num, (r, c)) -> r) ls)
      in filter (\(num, pos) -> not (inSameSquare (minPos ls) pos || inSameSquare (maxPos ls) pos))
        [(number, (r, c)) | r <- rows, c <- [1..9]]

-- TODO generalize to include squareColumnInteractions
squareSquareInteractionsColumn :: Candidates -> [(Int, Position)]
squareSquareInteractionsColumn candidates = filter (candidateIsPresent candidates) $
  concatMap (concatMap toRemovableCandidates . filter coverTwoColumns . groupByNum . joinCandidates candidates) squares
  where
    squareNums = [(1,2), (1,3), (2,3), (4,5), (4,6), (5,6), (7,8), (7,9), (8,9),
                  (1,4), (1,7), (4,7), (2,5), (2,8), (5,8), (3,6), (3,9), (6,9)]
    squares = map (\(i,j) -> (squaresPos !! (i-1) ++ squaresPos !! (j-1))) squareNums
    groupByNum = groupBy (\a b -> (fst a) == (fst b)) . sortBy (comparing fst)
    minPos ls = minimum (map snd ls)
    maxPos ls = maximum (map snd ls)
    coverTwoColumns ls = length (nub $ map (\(num, (r, c)) -> c) ls) == 2 && (not (inSameSquare (minPos ls) (maxPos ls)))
    toRemovableCandidates ls =
      let
          number = head (map fst ls)
          columns = nub (map (\(num, (r, c)) -> c) ls)
      in filter (\(num, pos) -> not (inSameSquare (minPos ls) pos || inSameSquare (maxPos ls) pos))
        [(number, (r, c)) | r <- [1..9], c <- columns]

inSameSquare :: Position -> Position -> Bool
inSameSquare (r1, c1) (r2, c2) = floor3 r1 == floor3 r2 && floor3 c1 == floor3 c2
  where floor3 n = ((n - 1) `div` 3) * 3 + 1

joinCandidates :: Candidates -> [Position] -> [(Int, Position)]
joinCandidates candidates positions = flatten $ joinCandidateSets candidates positions

joinCandidateSets :: Candidates -> [Position] -> [(IntSet, Position)]
joinCandidateSets candidates positions = [(getElem i j candidates, (i, j)) | (i, j) <- positions]

flatten :: [(IntSet, Position)] -> [(Int, Position)]
flatten list = concat (map flat list)
  where flat (set, pos) = map (\n -> (n, pos)) (elems set)

-- Updates the candidates with newly found numbers
updateCandidates :: Candidates -> [(Int, Position)] -> Candidates
updateCandidates candidates newNumbers = foldl f candidates newNumbers
  where f candidates (num, pos) = removeNeighbourCandidates num pos candidates

-- Removes all alternate candidates of Int around Pos
removeNeighbourCandidates :: Int -> Position -> Candidates -> Candidates
removeNeighbourCandidates x pos candidates = removeAllCandidates pos neighbourCandidatesRemoved
  where
    neighbourCandidatesRemoved = foldl (\acc pos -> removeCandidate x pos acc) candidates (getNeighbours pos)

-- Removes multiple candidates
removeCandidates :: Candidates -> [(Int, Position)] -> Candidates
removeCandidates candidates found = foldl f candidates found
  where f candidates (num, pos) = removeCandidate num pos candidates

-- Removes candidate Int at Position
removeCandidate :: Int -> Position -> Candidates -> Candidates
removeCandidate number pos candidates = mapElemMatrix (IntSet.delete number) pos candidates

-- Removes all candidates at Position
removeAllCandidates :: Position -> Candidates -> Candidates
removeAllCandidates pos candidates = Matrix.setElem IntSet.empty pos candidates

-- Looks whether given candidate is present
candidateIsPresent :: Candidates -> (Int, Position) -> Bool
candidateIsPresent candidates (num, (r, c)) = IntSet.member num (getElem r c candidates)


-- Shows candidates as a matrix
showCandidates :: Candidates -> String
showCandidates candidates = show (fmap IntSet.toList candidates)