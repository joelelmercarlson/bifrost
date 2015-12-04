module Control (movement) where

import qualified Graphics.UI.GLFW as GLFW

import Control.Applicative
import Data.Fixed
import Data.Maybe

import Control.Lens
import Linear
import Linear.Vector

import Entity

movement :: GLFW.Window -> Entity -> IO Entity
movement w e = do
  t               <- fromMaybe 0 <$> GLFW.getTime
  (width, height) <- GLFW.getFramebufferSize w
  (mx, my)        <- GLFW.getCursorPos w
  (cx, cy)        <- getCursorKeyDirections w

  GLFW.setCursorPos w 0 0
  GLFW.setCursorInputMode w GLFW.CursorInputMode'Hidden

  let dt = realToFrac (t - lasttime e)
      ms = mouseSpeed e
      sp = speed e
      -- | mouse movement
      pitch = clampPitch (xAngle e) (ms * realToFrac mx)
      yaw   = clampYaw   (yAngle e) (ms * realToFrac my)
      -- | orientation
      qx = axisAngle (V3 1 0 0) pitch -- x-axis
      qy = axisAngle (V3 0 1 0) yaw   -- y-axis
      q  = qy * qx
      -- | position
      direction = rotate q (V3 0 0 (-1)) ^* (sp * dt * realToFrac cy)
      right     = rotate q (V3 1 0 0)    ^* (sp * dt * realToFrac cx)
      current   = clamp (position e + direction + right)

  return Entity { lasttime = t
                , position = current
                , orientation = q
                , xAngle = pitch
                , yAngle = yaw
                , speed = sp
                , mouseSpeed = ms
                }

-- | clamp world coordinates
clamp :: V3 Float -> V3 Float  
clamp v = V3 x' y' z'
  where
    maxX = 10.0
    maxY = 2.0
    maxZ = 10.0
    x = v ^. _x
    y = v ^. _y
    z = v ^. _z
    x'
      | x > maxX    = maxX
      | x < (-maxX) = -maxX
      | otherwise   = x
    y'
      | y > maxY    = maxY
      | y < (-maxY) = -maxY
      | otherwise   = y
    z'
      | z > maxZ    = maxZ
      | z < (-maxZ) = -maxZ
      | otherwise   = z

-- | clampPitch normalize angle
clampPitch :: Float -> Float -> Float
clampPitch xa right = pitch
  where
    x     = mod' (xa + right) maxX
    maxX  = 360.0
    pitch
      | x > maxX    = x - maxX
      | x < (-maxX) = x + maxX
      | otherwise   = x

-- | clampYaw normalize angle
clampYaw :: Float -> Float -> Float
clampYaw ya up = yaw
  where
    y    = ya + up
    maxY = 85.0
    yaw
      | y > maxY    = maxY
      | y < (-maxY) = -maxY
      | otherwise   = y

-- | keyboard
isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False

getCursorKeyDirections :: GLFW.Window -> IO (Double, Double)
getCursorKeyDirections win = do
  x0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Left 
  x1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Right 
  y0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Up 
  y1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Down 
  let x0n = if x0 then (-1) else 0
      x1n = if x1 then   1  else 0
      y0n = if y0 then (-1) else 0
      y1n = if y1 then   1  else 0
  return (x0n + x1n, y0n + y1n)
