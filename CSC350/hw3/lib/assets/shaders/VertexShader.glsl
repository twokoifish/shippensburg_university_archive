// Vertex Shader.
#version 410
#extension GL_ARB_explicit_attrib_location : require

// Use location 0 for position (defined in CPP file).
layout(location = 0) in vec3 vPosition; // X, Y, Z
// Use location 1 for color (defined in CPP file).
layout(location = 1) in vec4 vColor; // R, G, B
// Use location 2 for surface normal (defined in CPP file).
layout(location = 2) in vec3 vNormal;

// Import the transfromation matrix from our cpp.
uniform mat4 transformedMatrix;
uniform mat4 rotationMatrix;

// Output the color data to the fragment shader.
out vec4 fragPos;
out vec4 fragColor;
out vec4 Normal;

void main()
{
  vec4 position = transformedMatrix * vec4(vPosition, 1.0);
  // Adjust each vector by the transformation matrix.
  gl_Position = position;
  fragPos = position;
  Normal = vec4(vNormal, 1.0) * inverse(rotationMatrix);
  // Set the outgoing glColor to the incoming vecColor variable.
  fragColor = vColor;
}
