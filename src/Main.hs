module Main where

import Control.Applicative
import Control.Monad (unless, when)
import Data.Maybe
import qualified Graphics.UI.GLFW as GLFW
import System.Environment
import System.Exit
import System.IO

import Draw2
import Control (movement)
import Entity
import Util (nth)

main :: IO ()
main = do
  xs <- getArgs
  let json = fromMaybe cube (nth 1 xs)

  withWindow width height "bifrost" $ \win -> do
    GLFW.setErrorCallback   $ Just errorCallback
    GLFW.setKeyCallback win $ Just keyCallback

    prog <- initResources json

    mainLoop prog win entity

    GLFW.destroyWindow win
    GLFW.terminate
    exitSuccess
  where
    width  = 800
    height = 600
    entity = initEntity
    cube   = "json/cube.json"

-- | forever
mainLoop :: Resources -> GLFW.Window -> Entity -> IO ()
mainLoop r w e = do
  draw r w e
  GLFW.swapBuffers w
  GLFW.pollEvents
  f <- movement w e
  q <- GLFW.windowShouldClose w
  unless q $ mainLoop r w f

-- | window creation
withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

-- | event callBacks
errorCallback :: GLFW.ErrorCallback
errorCallback err = hPutStrLn stderr

keyCallback :: GLFW.KeyCallback
keyCallback window key scancode action mods = do
  when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $ GLFW.setWindowShouldClose window True
  when (key == GLFW.Key'Q && action == GLFW.KeyState'Pressed) $ GLFW.setWindowShouldClose window True
