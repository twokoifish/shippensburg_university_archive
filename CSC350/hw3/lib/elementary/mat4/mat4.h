/**
 * @file Mat4.h
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Allows for the creation of a custom 4x4 matrix class.
 * @version 0.1
 * @date 2021-09-29
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef Mat4_H // If there is no header defined for Mat4, then define one.
#define Mat4_H

#include <iostream> // Header for cout and endl
#include <cmath> // Header for cos, sin, and tan.

#include "../../../hw3.h"

#define ROW_SIZE 4 // Matrix row size (int).
#define COL_SIZE 4 // Matrix column size (int).
#define HALF_UC 180.0 // Half of the unit circle.
#define MATRIX_DEBUG 0 // Debug mode toggle.



using namespace std; // Set namespace to avoid "std::" prefix.

/**
 * @brief Class that allows for a custom 4x4 matrix.
 * 
 */
class Mat4
{

private:
  /**
   * @brief The data in the matrix.
   * 
   */
  float data[ROW_SIZE][COL_SIZE];

public:
  /**
   * @brief Construct a new Matrix object
   * 
   */
  Mat4(void);

  /**
   * @brief Construct a new Matrix object
   * 
   * @param array the data to convert to a matrix.
   */
  Mat4(float array[ROW_SIZE][COL_SIZE]);

  /**
   * @brief Get a row of floats from the matrix.
   * 
   * @param i the row index to pull.
   * @return float* the row.
   */
  float *operator[](int i);

  /**
   * @brief Multiply 2 matrices together.
   * 
   * @param operand the other matrix to multiply by.
   * @return Matrix the product of those two matrices.
   */
  Mat4 operator*(Mat4 &operand);

  /**
   * @brief Print the content of a given matrix.
   * 
   */
  void print(void);

  /**
   * @brief Convert an angle from degrees to radians.
   * 
   * @param degrees to be converted to radians.
   * @return float angle from degrees to radians.
   */
  static float convertToRadians(float degrees);

  /**
   * @brief Creates and identity matrix.
   * 
   * @return Matrix that only contains a 1 diagonal.
   */
  static Mat4 identity(void);

  /**
   * @brief Create a matrix with scaling on the X axis.
   * 
   * @param factor to scale the X axis by.
   * @return Matrix with scaling on the X axis.
   */
  static Mat4 scaleX(float factor);

  /**
   * @brief Create a matrix with scaling on the Y axis.
   * 
   * @param factor to scale the Y axis by.
   * @return Matrix with scaling on the Y axis.
   */
  static Mat4 scaleY(float factor);

  /**
   * @brief Create a matrix with scaling on the Z axis.
   * 
   * @param factor to scale the Z axis by.
   * @return Matrix with scaling on the Z axis.
   */
  static Mat4 scaleZ(float factor);

  /**
   * @brief Create a matrix with uniform scaling on all axis.
   * 
   * @param uniformFactor to scale all axis by.
   * @return Matrix with uniform scaling on all axis.
   */
  static Mat4 scaleUniform(float uniformFactor);

  /**
   * @brief Create a matrix with scaling on multiple axis.
   * 
   * @param xFactor to scale the X axis by.
   * @param yFactor to scale the Y axis by.
   * @param zFactor to scale the Z axis by.
   * @return Matrix with scaling on multiple axis.
   */
  static Mat4 scale(float xFactor, float yFactor, float zFactor);

  /**
   * @brief Create a matrix with a translation on the X axis.
   * 
   * @param units to translate by on the X axis.
   * @return Matrix with a translation on the X axis.
   */
  static Mat4 translateX(float units);

  /**
   * @brief Create a matrix with a translation on the Y axis.
   * 
   * @param units to translate by on the Y axis.
   * @return Matrix with a translation on the Y axis.
   */
  static Mat4 translateY(float units);

  /**
   * @brief Create a matrix with a translation on the Z axis.
   * 
   * @param units to translate by on the Z axis.
   * @return Matrix with a translation on the Z axis.
   */
  static Mat4 translateZ(float units);

  /**
   * @brief Create a matrix with a uniform translation on all axis.
   * 
   * @param uniformUnits to translate by on all axis.
   * @return Matrix with a uniform translation on all axis.
   */
  static Mat4 translateUniform(float uniformUnits);

  /**
   * @brief Create a matrix with translations on multiple axis.
   * 
   * @param xUnits to translate by on the X axis.
   * @param yUnits to translate by on the Y axis.
   * @param zUnits to translate by on the Z axis.
   * @return Matrix with translations on multiple axis.
   */
  static Mat4 translate(float xUnits, float yUnits, float zUnits);

  /**
   * @brief Create a matrix with a rotation around the X axis.
   * 
   * @param degrees to rotate around the X axis.
   * @return Matrix with a rotation around the X axis.
   */
  static Mat4 rotateX(float degrees);

  /**
   * @brief Create a matrix with a rotation around the Y axis.
   * 
   * @param degrees to rotate around the Y axis.
   * @return Matrix with a rotation around the Y axis.
   */
  static Mat4 rotateY(float degrees);

  /**
   * @brief Create a matrix with a rotation around the z axis.
   * 
   * @param degrees to rotate around the Z axis.
   * @return Matrix with a rotation around the Z axis.
   */
  static Mat4 rotateZ(float degrees);

  /**
   * @brief Create a matrix with a uniform rotation on all axis.
   * 
   * @param uniformDegrees to rotate around all axis.
   * @return Matrix with a uniform rotation on all axis.
   */
  static Mat4 rotateUniform(float uniformDegrees);

  /**
   * @brief Create a matrix with rotations around multiple axis.
   * 
   * @param xDegrees to rotate around the X axis.
   * @param yDegrees to rotate around the Y axis.
   * @param zDegrees to rotate around the Z axis.
   * @return Matrix with rotations around multiple axis.
   */
  static Mat4 rotate(float xDegrees, float yDegrees, float zDegrees);

  /**
    * @brief Create a perspective matrix for the UI.
    * 
    * @param far the largest point of the perspective.
    * @param near the smallest point of the perspective.
    * @param degrees the fov.
    * @return Mat4 with the perspective adjusted for the UI.
    */
  static Mat4 perspectiveProjection(float far, float near, float degrees);
};

#endif