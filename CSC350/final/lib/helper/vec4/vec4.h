/**
 * @file vec4.h
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Template for a vec4 class.
 * @version 0.1
 * @date 2021-10-26
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef VEC4_H
#define VEC4_H

#define VEC4_LEN 4
#define VEC4_DISTANCE 0.5
#define VEC4_IOB(i) "vec4 index out of bounds: \n\tlength = " << VEC4_LEN << "; index = " << i

// Built-in libraries
#include <iostream>
#include <cmath>

using namespace std;

/**
 * @brief A floating point vector with four components.
 * 
 */
class vec4
{

public:
  float x; // The x component of the vec4.
  float y; // The y component of the vec4.
  float z; // The z component of the vec4.
  float w; // The w component of the vec4.

  /**
   * @brief Constructs a new vec4 object.
   * 
   */
  vec4(void);

  /**
   * @brief Construct a new vec4 object
   * 
   * @param value The value for all components of the vec4/
   */
  vec4(float value);

  /**
   * @brief Constructs a new vec4 object
   * 
   * @param x The x component of the vec4.
   * @param y The y component of the vec4.
   * @param z The z component of the vec4.
   */
  vec4(float x, float y, float z);

  /**
   * @brief Calculates the summation of two vec4.
   * 
   * @param operand The vec4 to apply.
   * @return The summation of two vec4. 
   */
  vec4 operator+(vec4 &perand);

  /**
   * @brief Calculates the difference of two vec4.
   * 
   * @param operand The vec4 to apply.
   * @return The difference of two vec4. 
   */
  vec4 operator-(vec4 &operand);

  /**
   * @brief Calculates the scalar product of two vec4.
   * 
   * @param operand The vec4 to apply.
   * @return The scalar product of two vec4. 
   */
  float operator*(vec4 &operand);

  /**
   * @brief Calculates the cross product of two vec4.
   * 
   * @param operand The vec4 to apply.
   * @return The cross product of two vec4. 
   */
  vec4 operator%(vec4 operand);

  /**
   * @brief Calculates the midpoint of two vec4.
   * 
   * @param operand The vec4 to apply.
   * @return The midpoint of two vec4. 
   */
  vec4 operator|(vec4 &operand);

  /**
   * @brief Returns a component of a vec4.
   * 
   * @param index The component of the vec4 to return.
   *              Where 0 = x, 1 = y, etc.
   * @return A component of a vec4. 
   */
  float operator[](int index);

  /**
   * @brief Calculates the product of a vec4 and scalar.
   * 
   * @param operand The vec4 to apply.
   * @return The product of a vec4 and scalar. 
   */
  vec4 operator*(float operand);

  /**
   * @brief Calculates a normalized vec4.
   * 
   * @return A normalized vec4.
   */
  vec4 operator!(void);

  /**
   * @brief Calculates the length of a vec4.
   * 
   * @return The length of a vec4. 
   */
  float length(void);

  /**
   * @brief Prints the components of a vec4.
   * 
   */
  void print(void);

};

#endif //VEC4_H