OpenGL in Haskell
-----------------

app/Main.hs       -- main

images/           -- chrome.png

json/             -- Assets JSON

obj/              -- wavefront OBJ

shaders/          -- opengl glsl (lighting, textures)

src/Control.hs    -- Mouse/Keyboard movement

src/Draw2.hs      -- Texture Drawing from Wavefront .OBJ models

src/Entity.hs     -- Moveable State

src/JsonLoader.hs -- JSON loader all Assets are described in JSON

src/ObjLoader.hs  -- OBJ loader for Blender exports

src/ObjUtil.hs    -- Utilities for OBJ

Requires:
  GLFW-b OpenGL GLUtil Linear aeson

Ubuntu:
  freeglut3-dev

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
