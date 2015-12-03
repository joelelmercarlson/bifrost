module Draw2 (initResources, Resources, draw) where

import Data.Map as Map
import Control.Applicative

-- import all OpenGL libraries qualified
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil           as U
import qualified Graphics.GLUtil.Camera3D  as U
import qualified Linear                    as L
--
import Entity
import JsonLoader
import ObjLoader
import ObjUtil

draw :: Resources -> GLFW.Window -> Entity -> IO ()
draw r win e = do
  GL.depthFunc  $= Just GL.Less
  GL.cullFace   $= Just GL.Back
  GL.clearColor $= GL.Color4 0.2 0.2 0.2 1.0
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  -- GL viewport
  (width, height) <- GLFW.getFramebufferSize win
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

  -- shader program
  GL.currentProgram $= (Just . U.program . shaderProgram $ r)
  U.enableAttrib (shaderProgram r) "vertex3D"
  U.enableAttrib (shaderProgram r) "vertexUV"
  U.enableAttrib (shaderProgram r) "vertexNormal"

  -- GL Texture
  GL.texture        GL.Texture2D $= GL.Enabled
  GL.textureBinding GL.Texture2D $= Just (texture r)

  -- Matrix: Model * View * Projection
  U.asUniform (mvpM width height e r) $ U.getUniform (shaderProgram r) "MVP"
  U.asUniform (modelM (worldPosition r)) $ U.getUniform (shaderProgram r) "M"
  U.asUniform (viewM e) $ U.getUniform (shaderProgram r) "V"

  -- Lighting: LightPosition, LightColor
  U.asUniform (lightPosition r) $ U.getUniform (shaderProgram r) "LightPosition"
  U.asUniform (lightColor r) $ U.getUniform (shaderProgram r) "LightColor"

  -- vertices
  GL.bindBuffer GL.ArrayBuffer $= Just (verts r)
  U.setAttrib (shaderProgram r) "vertex3D"
    GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0

  -- uvs
  GL.bindBuffer GL.ArrayBuffer $= Just (uvs r)
  U.setAttrib (shaderProgram r) "vertexUV"
    GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0

  -- normals
  GL.bindBuffer GL.ArrayBuffer $= Just (normals r)
  U.setAttrib (shaderProgram r) "vertexNormal"
    GL.ToFloat $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0

  -- elements, one index for all your triangles
  GL.bindBuffer GL.ElementArrayBuffer $= Just (elementBuffer r)
  (size, ptr, usage) <- GL.get (GL.bufferData GL.ElementArrayBuffer)
  U.drawIndexedTris (fromIntegral size)

  -- GLUtil does not yet provide a function to disable attributes
  GL.vertexAttribArray (U.getAttrib (shaderProgram r) "vertex3D") $= GL.Disabled
  GL.vertexAttribArray (U.getAttrib (shaderProgram r) "vertexUV") $= GL.Disabled
  GL.vertexAttribArray (U.getAttrib (shaderProgram r) "vertexNormal") $= GL.Disabled

-- | Represents the shader program and its input buffers
data Resources = Resources { shaderProgram :: U.ShaderProgram
                           , verts :: GL.BufferObject
                           , elementBuffer :: GL.BufferObject
                           , uvs :: GL.BufferObject
                           , normals :: GL.BufferObject
                           , texture :: GL.TextureObject
                           , worldPosition :: L.V3 GL.GLfloat
                           , lightPosition :: L.V3 GL.GLfloat
                           , lightColor :: L.V3 GL.GLfloat
                           }

-- | init our shaders
--
-- GLUtil helps out with the ShaderProgram type, which keeps track of
-- the 'AttribLocations' and 'UniformLocation's by name.
--
-- Requires: Asset.json
initResources :: String -> IO Resources
initResources fp = do
  (Right json)   <- loadJSON fp
  (Right result) <- loadOBJ (wavefront json)

  let Just v  = Map.lookup "v"  result -- vertices L.V3
      Just vt = Map.lookup "vt" result -- texcoord L.V2
      Just vn = Map.lookup "vn" result -- normals  L.V3
      Just f  = Map.lookup "f"  result -- faces    v/t/n
      v'      = fmap convertString2V3 v
      vt'     = fmap convertString2V2 vt
      vn'     = fmap convertString2V3 vn
      fv      = fmap convertString2FV f

  putStrLn $ "wavefront = " ++ show fp
  putStrLn $ "# v       = " ++ show (length v')
  putStrLn $ "# vt      = " ++ show (length vt')
  putStrLn $ "# vn      = " ++ show (length vn')
  putStrLn $ "# fv      = " ++ show (length fv)

  Resources <$> U.simpleShaderProgram (vertexshader json) (fragmentshader json)
            <*> U.fromSource GL.ArrayBuffer v'
            <*> U.fromSource GL.ElementArrayBuffer fv
            <*> U.fromSource GL.ArrayBuffer vt'
            <*> U.fromSource GL.ArrayBuffer vn'
            <*> loadTex (uvmap json)
            <*> convertString2V3' (worldposition json)
            <*> convertString2V3' (lightposition json)
            <*> convertString2V3' (lightcolor json)

-- | Model -> View -> Projection
mvpM :: Int -> Int -> Entity -> Resources -> L.M44 GL.GLfloat
mvpM width height e r = projection L.!*! view L.!*! model 
  where
    model      = modelM (worldPosition r)
    view       = viewM e
    projection = U.projectionMatrix (pi/4) aspect 0.1 100
    aspect     = fromIntegral width / fromIntegral height

-- | Model
modelM :: L.V3 GL.GLfloat -> L.M44 GL.GLfloat
modelM = L.mkTransformationMat eye3
  where
    eye3       = L.V3 (L.V3 1 0 0) (L.V3 0 1 0) (L.V3 0 0 1)

-- | View
viewM :: Entity -> L.M44 GL.GLfloat
viewM e = U.camMatrix cam
  where
    cam = U.dolly (realToFrac <$> position e) U.fpsCamera

loadTex :: FilePath -> IO GL.TextureObject
loadTex f = do
  tex <- either error id <$> U.readTexture f
  GL.textureFilter   GL.Texture2D      $= ((GL.Linear', Nothing), GL.Linear')
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Mirrored, GL.ClampToEdge)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Mirrored, GL.ClampToEdge)
  return tex
