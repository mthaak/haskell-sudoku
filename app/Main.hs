module Main where

import Board
import Solve
import Control.Monad.Writer (runWriter)

main :: IO ()
main = do
  board <- loadBoard
  let (finalBoard, logs) =  runWriter (solve board)
  mapM_ putStrLn logs
  putStrLn (replicate 120 '-')
  putStrLn "Final board: "
  putStrLn (showBoard finalBoard)

loadBoard :: IO Board
loadBoard = fmap readBoard (readFile "data/board_hardest.txt")