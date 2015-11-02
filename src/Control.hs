--
-- http://www.tomdalling.com/blog/modern-opengl/04-cameras-vectors-and-input/
--
module Control (movement) where

import Control.Applicative
import Data.Fixed
import Data.Maybe

-- import all OpenGL libraries qualified
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L
import           Linear.Vector ((^*))
--
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
      qx = L.axisAngle (L.V3 1 0 0) pitch -- x-axis
      qy = L.axisAngle (L.V3 0 1 0) yaw   -- y-axis
      q  = qy * qx
      -- | position
      direction = L.rotate q (L.V3 0 0 (-1)) ^* (sp * dt * realToFrac cy)
      right     = L.rotate q (L.V3 1 0 0)    ^* (sp * dt * realToFrac cx)
      current   = position e + direction + right

  return Entity { lasttime = t
                , position = current
                , orientation = q
                , xAngle = pitch
                , yAngle = yaw
                , speed = sp
                , mouseSpeed = ms
                }

-- | clampPitch, clampYaw normalize angles
clampPitch :: Float -> Float -> Float
clampPitch xa right = pitch
  where
    x     = mod' (xa + right) maxX
    maxX  = 360
    pitch
      | x > maxX    = x - maxX
      | x < (-maxX) = x + maxX
      | otherwise   = x

clampYaw :: Float -> Float -> Float
clampYaw ya up = yaw
  where
    y    = ya + up
    maxY = 85
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
