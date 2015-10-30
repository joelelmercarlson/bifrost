#version 120

// interpolated valutes from the vertex shader
varying vec3 Position_worldspace;
varying vec3 Normal_cameraspace;
varying vec3 EyeDirection_cameraspace;
varying vec3 LightDirection_cameraspace;
varying vec2 UV;

// values that stay constant for the whole mesh
uniform sampler2D tex;
uniform vec3 LightPosition;
uniform vec3 LightColor;

void main(void) {
  // Light emission properties, should be uniforms
  float LightPower = 50.0f;

  // Material properties
  vec3 MaterialDiffuseColor = texture2D(tex, UV).rgb;
  vec3 MaterialAmbientColor = vec3(0.1, 0.1, 0.1) * MaterialDiffuseColor;
  vec3 MaterialSpecularColor = vec3(0.3, 0.3, 0.3);

  // Distance to the light
  float distance = length(LightPosition - Position_worldspace);

  // Normal of the computed fragment in camera space
  vec3 n = normalize (Normal_cameraspace);

  // Direction of the light
  vec3 l = normalize (LightDirection_cameraspace);

  // Cosine of the angle between the normal and the light direction
  float cosTheta = clamp (dot(n,l), 0.0, 1.0);

  // Eye vector (towards camera)
  vec3 E = normalize(EyeDirection_cameraspace);

  // Reflect vector
  vec3 R = reflect(-l, n);

  // Cosigine of the angle between Eye vector and Reflect vector
  float cosAlpha = clamp(dot(E,R), 0.0, 1.0);

  vec3 color = 
    // Ambient: simulates indirect lighting
    MaterialAmbientColor + 
    // Diffuse: color of the object
    MaterialDiffuseColor * LightColor * LightPower * cosTheta/(distance*distance) + 
    // Specular: reflective
    MaterialSpecularColor * LightColor * LightPower * pow(cosAlpha,5)/(distance*distance);

  // output
  //gl_FragColor = texture2D(tex, UV);
  gl_FragColor = vec4(color, 1.0);
}
