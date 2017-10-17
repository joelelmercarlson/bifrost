#!/usr/bin/env stack
-- stack script --system-ghc --resolver lts-9.0 --package "process"
{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Exit
import System.IO
import System.Process 

main :: IO ()
main = do
  run <- callCommand "stack --system-ghc build"
  putStrLn $ "build: " ++ show run
  run <- callCommand "stack --system-ghc exec bifrost json/monkey.json"
  putStrLn $ "run file: " ++ show run
