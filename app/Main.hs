module Main where

import Board
import Solve

main :: IO ()
main = fmap (showPrettyBoard . solve) loadBoard >>= putStrLn

loadBoard :: IO Board
loadBoard = fmap readBoard (readFile "data/board.txt")