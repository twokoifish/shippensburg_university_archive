#include "vec3.h"

Vec3::Vec3(void)
{
  this->x = 0;
  this->y = 0;
  this->z = 0;
}

Vec3::Vec3(float array[VEC3_COMPONENTS])
{
  this->x = array[0];
  this->y = array[1];
  this->z = array[2];
}

Vec3::Vec3(float x, float y, float z)
{
  this->x = x;
  this->y = y;
  this->z = z;
}

Vec3 identity(void)
{
  return Vec3(1, 1, 1);
}

float Vec3::operator*(Vec3 &multiplier)
{
  return x * multiplier.x + y * multiplier.y + z * multiplier.z;
}

void Vec3::print(void)
{
  cout << "vec3[" << x << ", " << y << ", " << z << "]" << endl;
}

Vec3 Vec3::operator-(Vec3 &operand)
{
  return Vec3(x - operand.x, y - operand.y, z - operand.z);
}

Vec3 Vec3::cross(Vec3 V)
{
  float cx = (y * V.z) - (z * V.y);
  float cy = (z * V.x) - (x * V.z);
  float cz = (x * V.y) - (y * V.x);
  return Vec3(cx, cy, cz);
}
