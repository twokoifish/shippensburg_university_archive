// Vertex Shader.
#version 410
#extension GL_ARB_explicit_attrib_location : require

// Use location 0 for position (defined in CPP file).
layout(location = 0) in vec2 vecPosition;
// Use location 1 for color (defined in CPP file).
layout(location = 1) in vec3 vecColor;

// Output the color data to the fragment shader.
out vec3 glColor;

void main()
{
  // Set the OpenGL position to the incoming vec position, then set the z-axis and others.
  gl_Position = vec4(vecPosition, 0.0, 1.0);
  // Set the outgoing glColor to the incoming vecColor variable.
  glColor = vecColor;
}