// Vertex Shader.
#version 410
#extension GL_ARB_explicit_attrib_location : require

// Use location 0 for position (defined in CPP file).
layout(location = 0) in vec3 vecPosition; // X, Y, Z
// Use location 1 for color (defined in CPP file).
layout(location = 1) in vec4 vecColor; // R, G, B

// Import the transfromation matrix from our cpp.
uniform mat4 transformedMatrix;

// Output the color data to the fragment shader.
out vec4 glColor;

void main()
{
  // Adjust each vector by the transformation matrix.
  gl_Position = transformedMatrix * vec4(vecPosition, 1.0);
  // Set the outgoing glColor to the incoming vecColor variable.
  glColor = vecColor; // R, G, B
}