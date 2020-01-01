module Utils
  ( intSetHead
  , removeLineEndings
  , filterUniqueEqBy
  , mapElemMatrix
  , boolToInt
  , tuplify3
  , intSetPowerset
  , subsetsOfSize
  , splits
  , splitsMinSize
  ) where

import           Data.Set (Set)
import           qualified Data.Set as Set
import           Data.IntSet (IntSet, elems)
import           qualified Data.IntSet as IntSet
import           Data.Matrix (Matrix, getElem, setElem)
import           Data.Vector (Vector, (!), Vector, (//))
import           Data.Ord (comparing)
import           Data.Function (on)
import           Data.List (nub)
import           Data.List.Extra (groupBy, sortBy)
import           Control.Monad (filterM)

-- Returns the first element of an IntSet
intSetHead :: IntSet -> Int
intSetHead = head . elems

-- Removes all the line ending characters \n from a String
removeLineEndings :: String -> String
removeLineEndings = filter (/= '\n')

-- Keeps only the elements for which (a -> b) returns a unique value (appearing only once)
filterUniqueEqBy :: Ord b => (a -> b) -> [a] -> [a]
filterUniqueEqBy f = map head . filter (\g -> length g == 1) . groupBy eq . sortBy (comparing f)
  where eq = (==) `on` f

-- Maps a single element, given by its coordinates, in a matrix using the supplied function
mapElemMatrix :: (x -> x) -> (Int, Int) -> Matrix x -> Matrix x
mapElemMatrix f (i, j) matrix = setElem new (i, j) matrix
  where
    new = f old
    old = getElem i j matrix

-- Converts a bool to an integer
boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

-- Converts list of size 3 to tuple
tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)

-- Calculates the powerset of an IntSet
intSetPowerset :: IntSet -> [IntSet]
intSetPowerset = map setOfIntToIntSet . Set.toList . Set.powerSet . intSetToSetOfInt

-- IntSet -> Set Int
intSetToSetOfInt :: IntSet -> Set Int
intSetToSetOfInt = Set.fromList . IntSet.toList

-- Set Int -> IntSet
setOfIntToIntSet :: Set Int -> IntSet
setOfIntToIntSet = IntSet.fromList . Set.toList

-- Returns all subsets with given size
-- TODO could be optimised
subsetsOfSize :: Int -> IntSet -> [IntSet]
subsetsOfSize n intSet =  filter ((n ==) . IntSet.size) (intSetPowerset intSet)

-- Returns all possible unique splits of two exclusive parts (without empty parts)
splits :: Ord a => Set a -> [(Set a, Set a)]
splits set = nub [(x, Set.difference set x) | x <- Set.toList (Set.powerSet set), x /= Set.empty, x /= set]

-- Returns all possible splits of two non-overlapping parts with a minimum size
splitsMinSize :: Ord a => Int -> Set a -> [(Set a, Set a)]
splitsMinSize size set = filter (\(a, b) -> atLeastSize a && atLeastSize b) (splits set)
  where
    atLeastSize set = (Set.size set) >= size