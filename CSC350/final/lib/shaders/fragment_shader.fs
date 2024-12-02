
#version 410 core
#extension GL_ARB_explicit_attrib_location : require

in vec3 fPosition;
in vec3 fNormal;
in vec3 fColor;
 
out vec3 gColor;

const float ambientStrength = 0.2;
const float infiniteStrength = 0.5;
const float pointLightStrength = 0.3;
const float spotLightStrength = 0.3;
const vec3 viewPos = vec3(0.0, 0.0, -1.0);
const float specularStrength = 0.6;

vec3 ambientLight()
{
  // Default color for the ambient light
  vec3 color = vec3(0.5, 0.5, 0.5);
  vec3 ambientLight = (color * ambientStrength);
  return ambientLight;
}

vec3 pointLight()
{
  // Default color and pos for point light.
  vec3 color = vec3(1.0, 0.0, 1.0);
  vec3 lightPos = vec3(-25, 0, 20);

  // Diffuse light
  vec3 normal = fNormal;
  vec3 lightDir = normalize(lightPos - fPosition);

  float dis = length(lightPos - fPosition);
  float attenuation = clamp(4.0 / dis, 0.0, 1.0);

  float diff = max(dot(normal, lightDir), 0.0);
  vec3 diffuse = (diff * color);

  // Specular light
  vec3 viewDir = normalize(viewPos - fPosition);
  vec3 reflectDir = reflect(-lightDir, normal);
  float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32);
  vec3 specular = (specularStrength * spec * color);

  // Ambient light
  vec3 ambient = ambientLight();

  // Point light
  vec3 pointLight = ((diffuse + specular) * attenuation) * pointLightStrength;
  return pointLight;
}

vec3 infiniteLight()
{
  vec3 color = vec3(1.0, 1.0, 1.0);
  vec3 lightDir = vec3(normalize(-vec4(0, -1, 0, 0)));
  float diff = max(dot(fNormal, lightDir), 0.0);
  vec3 diffuse = (diff * color);

  // Specular light
  vec3 viewDir = normalize(viewPos - fPosition);
  vec3 reflectDir = reflect(-lightDir, fNormal);
  float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32);
  vec3 specular = (specularStrength * spec * color);

  // Ambient light
  vec3 ambient = ambientLight();

  // Infinite light
  vec3 infiniteLight = (diffuse + specular) * infiniteStrength;
  return infiniteLight;
}

vec3 spotLight()
{
  vec3 lightPos = vec3(0, 30, -1);
  vec3 direction = vec3(0, -20, 30);
  vec3 lightColor = vec3(1.0, 1.0, 1.0);
  float cutoff = cos(radians(15.0));

  vec3 lightDir = normalize(lightPos - fPosition);
  
  float theta = dot(lightDir, normalize(-direction));
  if (theta > cutoff)
  {
    vec3 normal = fNormal;
    float diff = max(dot(normal, lightDir), 0.0);
    vec3 diffuse = (diff * lightColor);

    vec3 viewDir = normalize(viewPos - fPosition);
    vec3 reflectDir = reflect(-lightDir, normal);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32);
    vec3 specular = (specularStrength * spec * lightColor);

    return (diffuse + specular) * spotLightStrength;
  }
  else
  {
    return vec3(0.0);
  }
}

void main()
{
  vec3 point = pointLight();
  vec3 infinite = infiniteLight();
  vec3 spot = spotLight();
  vec3 ambient = ambientLight();

  gColor = fColor * (ambient + point + infinite + spot);
}