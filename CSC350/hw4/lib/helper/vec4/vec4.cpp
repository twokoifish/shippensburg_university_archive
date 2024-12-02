/**
 * @file vec4.cpp
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Implementation of a vec4 class.
 * @version 0.1
 * @date 2021-10-26
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "vec4.h"

/**
 * @brief Constructs a new vec4 object.
 * 
 */
vec4::vec4(void)
{
  x = 0;
  y = 0;
  z = 0;
  w = 1;
}

/**
 * @brief Construct a new vec4 object
 * 
 * @param value The value for all components of the vec4/
 */
vec4::vec4(float value)
{
  x = value;
  y = value;
  z = value;
  w = 1;
}

/**
 * @brief Constructs a new vec4 object
 * 
 * @param x The x component of the vec4.
 * @param y The y component of the vec4.
 * @param z The z component of the vec4.
 */
vec4::vec4(float x, float y, float z)
{
  this->x = x;
  this->y = y;
  this->z = z;
  this->w = 1;
}

/**
 * @brief Calculates the summation of two vec4.
 * 
 * @param operand The vec4 to apply.
 * @return The summation of two vec4. 
 */
vec4 vec4::operator+(vec4 &operand)
{
  vec4 sum;
  sum.x = (x + operand.x);
  sum.y = (y + operand.y);
  sum.z = (z + operand.z);
  sum.w = 1;
  return sum;
}

/**
 * @brief Calculates the difference of two vec4.
 * 
 * @param operand The vec4 to apply.
 * @return The difference of two vec4. 
 */
vec4 vec4::operator-(vec4 &operand)
{
  vec4 diff;
  diff.x = (x - operand.x);
  diff.y = (y - operand.y);
  diff.z = (z - operand.z);
  diff.w = 1;
  return diff;
}

/**
 * @brief Calculates the scalar product of two vec4.
 * 
 * @param operand The vec4 to apply.
 * @return The scalar product of two vec4. 
 */
float vec4::operator*(vec4 &operand)
{
  float prod = 0.0;
  prod += (x * operand.x);
  prod += (y * operand.y);
  prod += (z * operand.z);
  return prod;
}

/**
 * @brief Calculates the cross product of two vec4.
 * 
 * @param operand The vec4 to apply.
 * @return The cross product of two vec4. 
 */
vec4 vec4::operator%(vec4 operand)
{
  vec4 cross;
  cross.x = (y * operand.z) - (z * operand.y);
  cross.y = (z * operand.x) - (x * operand.z);
  cross.z = (x * operand.y) - (y * operand.x);
  cross.w = 1;
  return cross;
}

/**
 * @brief Calculates the midpoint of two vec4.
 * 
 * @param operand The vec4 to apply.
 * @return The midpoint of two vec4. 
 */
vec4 vec4::operator|(vec4 &operand)
{
  vec4 mid;
  mid.x = (x + operand.x) / 2.0;
  mid.y = (y + operand.y) / 2.0;
  mid.z = (z + operand.z) / 2.0;
  return mid;
}

/**
 * @brief Returns a component of a vec4.
 * 
 * @param index The component of the vec4 to return.
 *              Where 0 = x, 1 = y, etc.
 * @return A component of a vec4. 
 */
float vec4::operator[](int index)
{
  switch (index)
  {
  default:
    cout << VEC4_IOB(index) << endl;
    exit(EXIT_FAILURE);
  case 0:
    return x;
  case 1:
    return y;
  case 2:
    return z;
  case 3:
    return w;
  }
}

/**
 * @brief Calculates the product of a vec4 and scalar.
 * 
 * @param operand The vec4 to apply.
 * @return The product of a vec4 and scalar. 
 */
vec4 vec4::operator*(float operand)
{
  vec4 prod;
  prod.x = (x * operand);
  prod.y = (y * operand);
  prod.z = (z * operand);
  prod.w = 1;
  return prod;
}

/**
   * @brief Calculates a normalized vec4.
   * 
   * @return A normalized vec4.
   */
vec4 vec4::operator!(void)
{
  vec4 normal;
  float length = this->length();
  normal.x = (x * VEC4_DISTANCE) / length;
  normal.y = (y * VEC4_DISTANCE) / length;
  normal.z = (z * VEC4_DISTANCE) / length;
  normal.w = 1;
  return normal;
}

/**
 * @brief Calculates the length of a vec4.
 * 
 * @return The length of a vec4. 
 */
float vec4::length(void)
{
  float length = sqrt(pow(x, 2) + pow(y, 2) + pow(z, 2));
  return length;
}

/**
 * @brief Prints the components of a vec4.
 * 
 */
void vec4::print(void)
{
  cout << "vec4 (" << x << ", " << y << ", " << z << ", " << w << ")" << endl;
}