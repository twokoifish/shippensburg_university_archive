
#ifndef VEC4_H
#define VEC4_H

#include <iostream>
#include <cmath>

#define VEC4_SIZE 4

using namespace std;

class vec4
{
  public:
  float x;
  float y;
  float z;
  float w;
  float normal;

  /**
   * @brief Construct a new vec4 object
   * 
   */
  vec4(void);

  /**
   * @brief 
   * 
   * @param operand 
   * @return vec4 
   */
  vec4 operator+(vec4 operand);

  /**
   * @brief 
   * 
   * @param operand 
   * @return vec4 
   */
  vec4 operator-(vec4 operand);

  /**
   * @brief 
   * 
   * @param operand 
   * @return vec4 
   */
  vec4 operator%(vec4 operand);

  /**
   * @brief 
   * 
   * @param operand 
   * @return vec4 
   */
  vec4 operator|(vec4 operand);

  /**
   * @brief 
   * 
   * @param operand 
   * @return float 
   */
  float operator*(vec4 operand);

  float *asArray(void);

  /**
   * @brief 
   * 
   */
  void print(void);
};

#endif