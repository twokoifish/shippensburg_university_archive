/**
 * @file mat4.h
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Template for a mat4 class.
 * @version 0.1
 * @date 2021-10-26
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef MAT4_H
#define MAT4_H

#define MAT4_ROW_LEN 4
#define MAT4_COL_LEN 4
#define MAT4_IOB(i) "mat4 index out of bounds: \n\tlength = " << MAT4_ROW_LEN << "; index = " << i

#include "../vec4/vec4.h"
#include <iostream>
#include <cmath>

using namespace std;

/**
 * @brief A type representing a 4x4 matrix with 16 floating point values.
 * 
 */
class mat4
{
  private:
  float data[MAT4_ROW_LEN][MAT4_COL_LEN];

  public:

  /**
   * @brief Construct a new mat4 object
   * 
   */
  mat4(void);

  /**
   * @brief Construct a new mat4 object
   * 
   * @param array Data used to build a mat4.
   */
  mat4(float array[MAT4_ROW_LEN][MAT4_COL_LEN]);

  /**
   * @brief Returns a row of a mat4.
   * 
   * @param index The row of the mat4 to return.
   * @return A row of a mat4.
   */
  float *operator[](int index);

  /**
   * @brief Calculates the summation of two mat4.
   * 
   * @param operand The mat4 to apply.
   * @return The summation of two mat4. 
   */
  mat4 operator+(mat4 operand);

  /**
   * @brief Calculates the difference of two mat4.
   * 
   * @param operand The mat4 to apply.
   * @return The difference of two mat4. 
   */
  mat4 operator-(mat4 operand);

  /**
   * @brief Calculates the product of two mat4.
   * 
   * @param operand The mat4 to apply.
   * @return The product of two mat4. 
   */
  mat4 operator*(mat4 operand);
  
  /**
   * @brief Calculates the product of a scalar and mat4.
   * 
   * @param operand The scalar to apply.
   * @return The product of a scalar and mat4.
   */
  mat4 operator*(float operand);

  /**
   * @brief Prints the components of a mat4.
   * 
   */
  void print(void);
};

#endif //MAT4_H