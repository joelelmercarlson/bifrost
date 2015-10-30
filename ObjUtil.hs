module ObjUtil (
    convertString2V3 
  , convertString2V3' 
  , convertString2V2 
  , convertString2FV 
  ) where

import Data.List.Split
import qualified Linear as L
import qualified Graphics.Rendering.OpenGL as GL

-- | IO GL.GLfloat
convertString2V3' :: String -> IO (L.V3 GL.GLfloat)
convertString2V3' str = do
  let ( x : y : z : _ ) = words str
  let x' = read x :: GL.GLfloat
      y' = read y :: GL.GLfloat
      z' = read z :: GL.GLfloat
  return (L.V3 x' y' z')

convertString2V2 :: String -> L.V2 Float
convertString2V2 str = do
  let ( x : y : _ ) = words str
  let x' = read x :: Float
      y' = read y :: Float
  L.V2 x' y'

convertString2V3 :: String -> L.V3 Float
convertString2V3 str = do
  let ( x : y : z : _ ) = words str
  let x' = read x :: Float
      y' = read y :: Float
      z' = read z :: Float
  L.V3 x' y' z'

-- | convertString2F[V,T,N]
-- | .obj faces index at 1, haskell is 0
-- | vertice / texture / normal
convertString2FV :: String -> L.V3 GL.GLuint
convertString2FV str = do
  let ( f0 : f1 : f2 : _ ) = words str
  let x = splitOn "/" f0
      y = splitOn "/" f1 
      z = splitOn "/" f2 
      v0 = read (head x) - 1
      v1 = read (head y) - 1
      v2 = read (head z) - 1
  L.V3 v0 v1 v2
