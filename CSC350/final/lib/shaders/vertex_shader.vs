
#version 410 core
#extension GL_ARB_explicit_attrib_location : require

layout (location = 0) in vec4 vPosition;
layout (location = 1) in vec4 vNormal;

uniform mat4 mVertexTransformation;
uniform mat4 mNormalTransformation;

uniform vec4 vColor;

out vec3 fPosition;
out vec3 fNormal;
out vec3 fColor;

void main()
{
  vec4 position = mVertexTransformation * vPosition;
  vec4 nNorm = mNormalTransformation * vNormal;
  vec3 normal = vec3(nNorm);
  gl_Position = position;
  fPosition = vec3(position);
  fNormal = normalize(normal);
  fColor = vec3(vColor);
}