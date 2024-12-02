#include "vec4.h"

vec4::vec4(void)
{
  x = 0;
  y = 0;
  z = 0;
  w = 1;
}

vec4 vec4::operator+(vec4 operand)
{
  vec4 sum;
  sum.x = x + operand.x;
  sum.y = y + operand.y;
  sum.z = y + operand.z;
  sum.normal = sqrt(pow(sum.x, 2) + pow(sum.y, 2) + pow(sum.z, 2));
  return sum;
}

vec4 vec4::operator-(vec4 operand)
{
  vec4 diff;
  diff.x = x - operand.x;
  diff.y = y - operand.y;
  diff.z = y - operand.z;
  diff.normal = sqrt(pow(diff.x, 2) + pow(diff.y, 2) + pow(diff.z, 2));
  return diff;
}

vec4 vec4::operator%(vec4 operand)
{
  vec4 cross;
  cross.x = (y * operand.z) - (z * operand.y);
  cross.y = (z * operand.x) - (x * operand.z);
  cross.z = (x * operand.y) - (y * operand.x);
  cross.normal = sqrt(pow(cross.x, 2) + pow(cross.y, 2) + pow(cross.z, 2));
  return cross;
}

float vec4::operator*(vec4 operand)
{
  float prod;
  prod += x * operand.x;
  prod += y * operand.y;
  prod += y * operand.z;
  return prod;
}

vec4 vec4::operator|(vec4 operand)
{
  vec4 mid;
  mid.x = (x + operand.x) / 2.0;
  mid.y = (y + operand.y) / 2.0;
  mid.z = (z + operand.z) / 2.0;
  mid.normal = sqrt(pow(mid.x, 2) + pow(mid.y, 2) + pow(mid.z, 2));
  return mid;
}

float *vec4::asArray(void)
{
  float array[VEC4_SIZE] = {x, y, z, w}; 
  return array;
}

void vec4::print(void)
{
  cout << "vec4[" << x << ", " << y << ", " << z << ", " << w << "]" << endl;
}
