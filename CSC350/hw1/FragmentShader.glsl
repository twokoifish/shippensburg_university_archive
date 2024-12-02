// Fragment Shader.
#version 410
#extension GL_ARB_explicit_attrib_location : require

// The incoming color data.
in vec3 glColor;

// The outgoing color data to the GPU.
out vec4 outColor;

void main()
{
  // Set the outgoing color data to the incoming color data, add alpha.
  outColor = vec4(glColor, 1.0);
}
