module Main where

import Data.Map as Map
import qualified Linear as L

import ObjUtil
import ObjLoader
import JsonLoader

main :: IO ()
main = do
  (Right json) <- loadJSON "json/cube.json"
  (Right result) <- loadOBJ (wavefront json)
  let Just v  = Map.lookup "v"  result        -- vertices
      Just vt = Map.lookup "vt" result        -- texcoords
      Just vn = Map.lookup "vn" result        -- normals
      Just f  = Map.lookup "f"  result        -- faces
      v'      = fmap convertString2V3 v
      vt'     = fmap convertString2V2 vt
      vn'     = fmap convertString2V3 vn
      fv      = fmap convertString2FV f   -- vertice / texture / normal
      ft      = fmap convertString2FT f
      fn      = fmap convertString2FN f 
      ps      = convertString2V3 (worldposition json)
      lp      = convertString2V3 (lightposition json)
      lc      = convertString2V3 (lightcolor json)
  
  putStrLn $ "v  = " ++ show v'
  putStrLn $ "vt = " ++ show vt'
  putStrLn $ "vn = " ++ show vn'
  putStrLn $ "fv = " ++ show fv
  putStrLn $ "worldposition = " ++ show ps
  putStrLn $ "lightposition = " ++ show lp
  putStrLn $ "ligthcolor = " ++ show lc
