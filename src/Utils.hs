module Utils
  ( intSetHead
  , removeLineEndings
  , filterUniqueEqBy
  , mapElemMatrix
  , mapElemVector
  , boolToInt
  ) where

import           Data.IntSet (IntSet, elems)
import           Data.Matrix (Matrix, getElem, setElem)
import           Data.Vector (Vector, (!), Vector, (//))
import           Data.Ord (comparing)
import           Data.Function (on)
import           Data.List.Extra (groupBy, sortBy)

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

-- Maps a single element, given by its index, in a vector using the supplied function
mapElemVector :: (x -> x) -> Int -> Vector x -> Vector x
mapElemVector f i vector = vector // [(i, new)]
  where
    new = f old
    old = vector ! i

-- Convert a bool to an integer
boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0