// Fragment Shader.
#version 410
#extension GL_ARB_explicit_attrib_location : require

// The incoming color data.
in vec4 Normal;
in vec4 fragColor;
in vec4 fragPos;

// The outgoing color data to the GPU.
out vec4 GPUCol;

void main()
{
  vec4 ambientStrength = vec4(0.1, 0.1, 0.1, 1.0);

  vec4 lightPos = vec4(-2.5, 2.5, 5, 1.0);
  vec4 lightCol = vec4(1.0, 1.0, 1.0, 1.0);

  vec4 viewPos = vec4(0.0, 0.0, -1.0, 1.0);

  vec4 normal = normalize(Normal);
  vec4 lightDir = normalize(fragPos - lightPos);
  float diff = max(dot(normal, lightDir), 0.0);
  vec4 diffuse = diff * lightCol;
  float specularStrength = 0.6;
  vec4 viewDir = normalize(fragPos - viewPos);
  vec4 reflectDir = reflect(-lightDir, normal);
  float spec = pow(max(dot(viewDir, reflectDir), 0.0), 128);
  vec4 specular = specularStrength * spec * lightCol;

  // Set the outgoing color data to the incoming color data, add alpha.
  GPUCol = fragColor * (diffuse + ambientStrength + specular);
  
}
