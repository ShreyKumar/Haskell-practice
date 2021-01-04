module Main where

import Lib
import Data
import System.IO
import System.Random

main :: IO ()
main = do
  gen <- newStdGen
  let filledInGrid = fillInBlanks gen grid
      game = makeGame filledInGrid languages
  hSetBuffering stdout NoBuffering
  playTurn game

playTurn game = do
    putStrLn . formatGame $ game
    putStr "Please enter a word> "
    word <- getLine
    putStrLn $ "You entered " ++ word
    let newGame = playGame game word
    if completed newGame then
      putStrLn "Congratulations!"
    else
      playTurn newGame

    putStrLn . formatGame $ newGame
    playTurn newGame
