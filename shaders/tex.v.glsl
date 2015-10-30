#version 120

// Input vertex data
attribute vec3 vertex3D;
attribute vec2 vertexUV;
attribute vec3 vertexNormal;

// output data; will be interpolated for each fragment
varying vec3 Position_worldspace;
varying vec3 Normal_cameraspace;
varying vec3 EyeDirection_cameraspace;
varying vec3 LightDirection_cameraspace;
varying vec2 UV;

// values that stay constant for whole mesh
uniform mat4 MVP;
uniform mat4 M;
uniform mat4 V;
uniform vec3 LightPosition;
uniform vec3 LightColor;

void main(void) {
  // Ouput position of the vertex in clip space: MVP * vertex3D
  gl_Position = MVP * vec4(vertex3D, 1.0);

  // UV of the vertex
  UV = vertexUV;

  // Position of vertex in worldspace: M * vertex3D
  Position_worldspace = (M * vec4(vertex3D, 1.0)).xyz;

  // Normal of vertex in camera space: V * M * vertexNormal
  Normal_cameraspace = (V * M * vec4(vertexNormal, 0.0)).xyz;

  // Vector from vertex to camera
  vec3 vertexPosition_cameraspace = (V * M * vec4(vertex3D, 1.0)).xyz;
  EyeDirection_cameraspace = vec3(0.0, 0.0, 0.0) - vertexPosition_cameraspace;
 
  // Vector from vertex to light
  vec3 LightPosition_cameraspace = (V * vec4(LightPosition, 1.0)).xyz;
  LightDirection_cameraspace = LightPosition_cameraspace + EyeDirection_cameraspace;
}
