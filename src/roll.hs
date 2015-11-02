module Main where

import Control.Monad
import Control.Monad.Trans.State
import System.Random

rollDie :: State StdGen Int
rollDie = state $ randomR (1,6)

rollDice :: State StdGen (Int, Int)
rollDice = liftM2 (,) rollDie rollDie

getRandomSeed :: IO Int
getRandomSeed = do
  randomSrc <- getStdGen 
  return $ fst $ random randomSrc

main :: IO ()
main = do
  seed <- getRandomSeed
  print $ evalState rollDice (mkStdGen seed)
