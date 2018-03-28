#!/usr/bin/env stack
-- stack script --system-ghc --resolver lts-9.0 --package "process"
module Main where

  import System.Environment
  import System.Exit
  import System.IO
  import System.Process

  main :: IO ()
  main = do
    xs  <- getArgs
    let cmd = "stack exec Main " ++ head xs
    run <- callCommand cmd
    putStrLn $ show run
