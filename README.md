# Modern OpenGL in Haskell

Modern OpenGL OBJ viewer written in haskell.

### Screenshot
![monkey.png](images/monkey.png)

### Code

```
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
src/Util.hs       -- Utilities
```

### Requires
* GLFW-b OpenGL GLUtil Linear aeson

### RHEL/CENTOS
```
make rhel
make
make run
```

Author: "Joel E Carlson" <joel.elmer.carlson@gmail.com>
