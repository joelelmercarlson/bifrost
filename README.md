OpenGL in Haskell
-----------------

Control.hs      -- Mouse/Keyboard movement
Draw2.hs        -- Texture Drawing from Wavefront .OBJ models
Entity.hs       -- Moveable State
intro.hs        -- main
JsonLoader.hs   -- JSON loader all Assets are described in JSON
ObjLoader.hs    -- OBJ loader for Blender exports
ObjTest.hs      -- Test Program for Loaders
ObjUtil.hs      -- Utilities to convert data
roll.hs         -- example random in haskell
json/           -- json definition of Assets
obj/            -- wavefront obj
images/         -- uvmap in .bmp
shaders/        -- opengl glsl (lighting, textures)

Requires:
cabal install glfw-b OpenGL GLUtil Linear aeson

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
