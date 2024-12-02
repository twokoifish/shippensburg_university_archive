
#version 410 core// GLSL version.
#extension GL_ARB_explicit_attrib_location : require // Apple garbage.

layout (location = 0) in vec4 vPosition;
layout (location = 1) in vec4 vNormal;

uniform mat4 vProjection;
uniform mat4 vTranslate;
uniform mat4 vRotate;
uniform mat4 vScale;
uniform vec4 vColor;

out vec3 fPosition;
 out vec3 fNormal;
out vec3 fColor;

void main()
{
  mat4 mvp = vProjection * (vTranslate * vRotate * vScale);
  vec4 position = mvp * vPosition;
  vec3 normal = vec3(vRotate * vScale * vNormal);
  gl_Position = position;
   fPosition = vec3(position);
   fNormal = normalize(normal);
  fColor = vec3(vColor);
}