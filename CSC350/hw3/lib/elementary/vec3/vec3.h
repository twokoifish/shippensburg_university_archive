#ifndef VEC3_H
#define VEC3_H

#define VEC3_COMPONENTS 3
#include <iostream> // Header for cout and endl

using namespace std; // Set namespace to avoid "std::" prefix.

class Vec3
{
  public:
  float x;
  float y;
  float z;
  Vec3(void);
  Vec3(float array[VEC3_COMPONENTS]);
  Vec3(float x, float y, float z);
  Vec3 identity(void);
  float operator*(Vec3 &multiplier);
  Vec3 operator-(Vec3 &operand);
  Vec3 cross(Vec3 b);
  float operator[](int index);
  void print(void);
};

#endif // VEC3_H