/**
 * @file Matrix.cpp
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Allows for the creation of a custom 4x4 matrix class.
 * @version 0.1
 * @date 2021-09-29
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "mat4.h"

/**
 * @brief Construct a empty matrix.
 * 
 */
Mat4::Mat4(void)
{
  for (int i = 0; i < ROW_SIZE; i++)
  {
    for (int j = 0; j < COL_SIZE; j++)
    {
      // Create a blank matrix.
      data[i][j] = 0;
    }
  }
}

/**
 * @brief Construct a new Matrix from a 2D array.
 * 
 * @param array the array to convert to a matrix.
 */
Mat4::Mat4(float array[ROW_SIZE][COL_SIZE])
{
  for (int i = 0; i < ROW_SIZE; i++)
  {
    for (int j = 0; j < COL_SIZE; j++)
    {
      // Map each index from the array to the matrix.
      data[i][j] = array[i][j];
    }
  }
}

/**
 * @brief Get a row of floats from the matrix.
 * 
 * @param i the row index to pull.
 * @return float* the row.
 */
float *Mat4::operator[](int i)
{ 
  // If i is greater than the size of the matrix, print an exception and return dud values.
  if (i >= ROW_SIZE)
  {
    cout << "Index array out of bounds.\n" << "Expected i < " << ROW_SIZE << " but was " << i << "." << endl;
    static float error[ROW_SIZE] = {0,0,0,0};
    return error;
  }
  // Else return an actual row.
  else
  {
    return data[i];
  }
}

/**
 * @brief Multiply 2 matrices together.
 * 
 * @param operand the other matrix to multiply by.
 * @return Matrix the product of those two matrices.
 */
Mat4 Mat4::operator*(Mat4 &operand)
{
  // The product matrix.
  Mat4 result;
  for (int i = 0; i < ROW_SIZE; i++)
  {
    for (int j = 0; j < ROW_SIZE; j++)
    {
      for (int k = 0; k < ROW_SIZE; k++)
      {
        // Multiply the 2 matrices together.
        result[i][j] += data[i][k] * operand[k][j];
      }
    }
  }
  // Return the product.
  return result;
}

/**
 * @brief Print the content of a given matrix.
 * 
 */
void Mat4::print(void)
{
  // Walk the rows.
  for (int i = 0; i < ROW_SIZE; i++)
  {
    cout << "[";
    // Walk the columns.
    for (int j = 0; j < COL_SIZE; j++)
    { 
      // If not last element, print with space.
      if (j != COL_SIZE - 1)
      {
        cout << data[i][j] << " ";
      }
      // Otherwise just print the data.
      else
      {
        cout << data[i][j];
      }
    }
    cout << "]" << endl;
  }
}

/**
 * @brief Convert an angle from degrees to radians.
 * 
 * @param degrees to be converted to radians.
 * @return float angle from degrees to radians.
 */
float Mat4::convertToRadians(float degrees)
{
  // Convert degrees to radians, then return it.
  return degrees * (M_PI / HALF_UC);
}

/**
 * @brief Creates and identity matrix.
 * 
 * @return Matrix that only contains a 1 diagonal.
 */
Mat4 Mat4::identity(void)
{
  // Build array to convert ot matrix.
  float array[ROW_SIZE][COL_SIZE] = {
      {1, 0, 0, 0},
      {0, 1, 0, 0},
      {0, 0, 1, 0},
      {0, 0, 0, 1}};
  // Convert array to matrix.
  Mat4 identity = Mat4(array);
  // If debug then print matrix.
  if (MATRIX_DEBUG)
  {
    cout << "identity" << endl;
    identity.print();
  }
  // Return scale.
  return identity;
}

/**
 * @brief Create a matrix with scaling on the X axis.
 * 
 * @param factor to scale the X axis by.
 * @return Matrix with scaling on the X axis.
 */
Mat4 Mat4::scaleX(float factor)
{
  // Build array to convert ot matrix.
  float array[ROW_SIZE][COL_SIZE] = {
      {factor, 0, 0, 0},
      {0, 1, 0, 0},
      {0, 0, 1, 0},
      {0, 0, 0, 1}};
  // Convert array to matrix.
  Mat4 scaleX = Mat4(array);
  // If debug then print matrix.
  if (MATRIX_DEBUG)
  {
    cout << "scaleX" << endl;
    scaleX.print();
  }
  // Return scale.
  return scaleX;
}

/**
 * @brief Create a matrix with scaling on the Y axis.
 * 
 * @param factor to scale the Y axis by.
 * @return Matrix with scaling on the Y axis.
 */
Mat4 Mat4::scaleY(float factor)
{
  // Build array to convert ot matrix.
  float array[ROW_SIZE][COL_SIZE] = {
      {1, 0, 0, 0},
      {0, factor, 0, 0},
      {0, 0, 1, 0},
      {0, 0, 0, 1}};
  // Convert array to matrix.
  Mat4 scaleY = Mat4(array);
  // If debug then print matrix.
  if (MATRIX_DEBUG)
  {
    cout << "scaleY" << endl;
    scaleY.print();
  }
  // Return scale.
  return scaleY;
}

/**
 * @brief Create a matrix with scaling on the Z axis.
 * 
 * @param factor to scale the Z axis by.
 * @return Matrix with scaling on the Z axis.
 */
Mat4 Mat4::scaleZ(float factor)
{
  // Build array to convert ot matrix.
  float array[ROW_SIZE][COL_SIZE] = {
      {1, 0, 0, 0},
      {0, 1, 0, 0},
      {0, 0, factor, 0},
      {0, 0, 0, 1}};
  // Convert array to matrix.
  Mat4 scaleZ = Mat4(array);
  // If debug then print matrix.
  if (MATRIX_DEBUG)
  {
    cout << "scaleZ" << endl;
    scaleZ.print();
  }
  // Return scale.
  return scaleZ;
}

/**
 * @brief Create a matrix with uniform scaling on all axis.
 * 
 * @param uniformFactor to scale all axis by.
 * @return Matrix with uniform scaling on all axis.
 */
Mat4 Mat4::scaleUniform(float uniformFactor)
{
  // Create a uniform scale by passing the same factor for all axis.
  Mat4 scaleUniform = Mat4::scale(uniformFactor, uniformFactor, uniformFactor);
  // Return uniform scale matrix.
  return scaleUniform;
}

/**
 * @brief Create a matrix with scaling on multiple axis.
 * 
 * @param xFactor to scale the X axis by.
 * @param yFactor to scale the Y axis by.
 * @param zFactor to scale the Z axis by.
 * @return Matrix with scaling on multiple axis.
 */
Mat4 Mat4::scale(float xFactor, float yFactor, float zFactor)
{
  // Get the base matrix.
  Mat4 scale = Mat4::identity();
  // If there is a scale on X, apply it.
  if (xFactor != 0)
  {
    Mat4 scaleX = Mat4::scaleX(xFactor);
    scale = scale * scaleX;
  }
  // If there is a scale on Y, apply it.
  if (yFactor != 0)
  {
    Mat4 scaleY = Mat4::scaleY(yFactor);
    scale = scale * scaleY;
  }
  // If there is a scale on Z, apply it.
  if (zFactor != 0)
  {
    Mat4 scaleZ = Mat4::scaleZ(zFactor);
    scale = scale * scaleZ;
  }
  // Print matrix if in debug mode.
  if (MATRIX_DEBUG)
  {
    cout << "scale:" << endl;
    scale.print();
  }
  // Return scales on multiple axis.
  return scale;
}

/**
 * @brief Create a matrix with a translation on the X axis.
 * 
 * @param units to translate by on the X axis.
 * @return Matrix with a translation on the X axis.
 */
Mat4 Mat4::translateX(float units)
{
  // Construct the array to convert to a matrix.
  float array[ROW_SIZE][COL_SIZE] = {
      {1, 0, 0, units},
      {0, 1, 0, 0},
      {0, 0, 1, 0},
      {0, 0, 0, 1}};
  // Convert array to matrix.
  Mat4 translateX = Mat4(array);
  // Print matrix if in debug mode.
  if (MATRIX_DEBUG)
  {
    cout << "translateX" << endl;
    translateX.print();
  }
  // Return translation on the X axis.
  return translateX;
}

/**
 * @brief Create a matrix with a translation on the Y axis.
 * 
 * @param units to translate by on the Y axis.
 * @return Matrix with a translation on the Y axis.
 */
Mat4 Mat4::translateY(float units)
{
  // Construct the array to convert to a matrix.
  float array[ROW_SIZE][COL_SIZE] = {
      {1, 0, 0, 0},
      {0, 1, 0, units},
      {0, 0, 1, 0},
      {0, 0, 0, 1}};
  // Convert array to matrix.
  Mat4 translateY = Mat4(array);
  // Print matrix if in debug mode.
  if (MATRIX_DEBUG)
  {
    cout << "translateY" << endl;
    translateY.print();
  }
  // Return translation on the Y axis.
  return translateY;
}

/**
 * @brief Create a matrix with a translation on the Z axis.
 * 
 * @param units to translate by on the Z axis.
 * @return Matrix with a translation on the Z axis.
 */
Mat4 Mat4::translateZ(float units)
{
  // Construct the array to convert to a matrix.
  float array[ROW_SIZE][COL_SIZE] = {
      {1, 0, 0, 0},
      {0, 1, 0, 0},
      {0, 0, 1, units},
      {0, 0, 0, 1}};
  // Convert array to matrix.
  Mat4 translateZ = Mat4(array);
  // Print matrix if in debug mode.
  if (MATRIX_DEBUG)
  {
    cout << "translateZ" << endl;
    translateZ.print();
  }
  // Return translation on the Z axis.
  return translateZ;
}

/**
 * @brief Create a matrix with a uniform translation on all axis.
 * 
 * @param uniformUnits to translate by on all axis.
 * @return Matrix with a uniform translation on all axis.
 */
Mat4 Mat4::translateUniform(float uniformUnits)
{
  // Create a uniform translation by passing the same units for all axis.
  Mat4 translateUniform = Mat4::translate(uniformUnits, uniformUnits, uniformUnits);
  // Return uniform translation matrix.
  return translateUniform;
}

/**
 * @brief Create a matrix with translations on multiple axis.
 * 
 * @param xUnits to translate by on the X axis.
 * @param yUnits to translate by on the Y axis.
 * @param zUnits to translate by on the Z axis.
 * @return Matrix with translations on multiple axis.
 */
Mat4 Mat4::translate(float xUnits, float yUnits, float zUnits)
{
  // Get the base matrix.
  Mat4 translate = Mat4::identity();
  // If there is a translation on X, apply it.
  if (xUnits != 0)
  {
    Mat4 translateX = Mat4::translateX(xUnits);
    translate = translate * translateX;
  }
  // If there is a translation on Y, apply it.
  if (yUnits != 0)
  {
    Mat4 translateY = Mat4::translateY(yUnits);
    translate = translate * translateY;
  }
  // If there is a translation on Z, apply it.
  if (zUnits != 0)
  {
    Mat4 translateZ = Mat4::translateZ(zUnits);
    translate = translate * translateZ;
  }
  // Print matrix if in debug mode.
  if (MATRIX_DEBUG)
  {
    cout << "translate:" << endl;
    translate.print();
  }
  // Return translations on multiple axis.
  return translate;
}

/**
 * @brief Create a matrix with a rotation around the X axis.
 * 
 * @param degrees to rotate around the X axis.
 * @return Matrix with a rotation around the X axis.
 */
Mat4 Mat4::rotateX(float degrees)
{
  // Convert degrees to radians.
  float radians = Mat4::convertToRadians(degrees);
  // Construct the array to convert to a matrix.
  float array[ROW_SIZE][COL_SIZE] = {
      {1, 0, 0, 0},
      {0, cos(radians), -sin(radians), 0},
      {0, sin(radians), cos(radians), 0},
      {0, 0, 0, 1}};
  // Convert the array into a matrix.
  Mat4 rotateX = Mat4(array);
  // Print matrix if in debug mode.
  if (MATRIX_DEBUG)
  {
    cout << "rotateX:" << endl;
    rotateX.print();
  }
  // Return rotation around X axis.
  return rotateX;
}

/**
 * @brief Create a matrix with a rotation around the Y axis.
 * 
 * @param degrees to rotate around the Y axis.
 * @return Matrix with a rotation around the Y axis.
 */
Mat4 Mat4::rotateY(float degrees)
{
  // Convert degrees to radians.
  float radians = Mat4::convertToRadians(degrees);
  // Construct the array to convert to a matrix.
  float array[ROW_SIZE][COL_SIZE] = {
      {cos(radians), 0, sin(radians), 0},
      {0, 1, 0, 0},
      {-sin(radians), 0, cos(radians), 0},
      {0, 0, 0, 1}};
  // Convert the array into a matrix.
  Mat4 rotateY = Mat4(array);
  // Print matrix if in debug mode.
  if (MATRIX_DEBUG)
  {
    cout << "rotateY:" << endl;
    rotateY.print();
  }
  // Return rotation around Y axis.
  return rotateY;
}

/**
 * @brief Create a matrix with a rotation around the z axis.
 * 
 * @param degrees to rotate around the Z axis.
 * @return Matrix with a rotation around the Z axis.
 */
Mat4 Mat4::rotateZ(float degrees)
{
  // Convert degrees to radians.
  float radians = Mat4::convertToRadians(degrees);
  // Construct the array to convert to a matrix.
  float array[ROW_SIZE][COL_SIZE] = {
      {cos(radians), -sin(radians), 0, 0},
      {sin(radians), cos(radians), 0, 0},
      {0, 0, 1, 0},
      {0, 0, 0, 1}};
  // Convert the array into a matrix.
  Mat4 rotateZ = Mat4(array);
  // Print matrix if in debug mode.
  if (MATRIX_DEBUG)
  {
    cout << "rotateZ:" << endl;
    rotateZ.print();
  }
  // Return rotation around Z axis.
  return rotateZ;
}

/**
 * @brief Create a matrix with a uniform rotation on all axis.
 * 
 * @param uniformDegrees to rotate around all axis.
 * @return Matrix with a uniform rotation on all axis.
 */
Mat4 Mat4::rotateUniform(float uniformDegrees)
{
  // Create a uniform rotation by passing the same degrees for all axis.
  Mat4 rotateUniform = Mat4::rotate(uniformDegrees, uniformDegrees, uniformDegrees);
  // Return uniform rotation.
  return rotateUniform;
}

/**
 * @brief Create a matrix with rotations around multiple axis.
 * 
 * @param xDegrees to rotate around the X axis.
 * @param yDegrees to rotate around the Y axis.
 * @param zDegrees to rotate around the Z axis.
 * @return Matrix with rotations around multiple axis.
 */
Mat4 Mat4::rotate(float xDegrees, float yDegrees, float zDegrees)
{
  // Create a base identity matrix to multiple rotations on.
  Mat4 rotate = Mat4::identity();
  // If there is a rotation around X, apply it.
  if (xDegrees != 0)
  {
    Mat4 rotateX = Mat4::rotateX(xDegrees);
    rotate = rotate * rotateX;
  }
  // If there is a rotation around Y, apply it.
  if (yDegrees != 0)
  {
    Mat4 rotateY = Mat4::rotateY(yDegrees);
    rotate = rotate * rotateY;
  }
  // If there is a rotation around Z, apply it.
  if (zDegrees != 0)
  {
    Mat4 rotateZ = Mat4::rotateZ(zDegrees);
    rotate = rotate * rotateZ;
  }
  // Print matrix if in debug mode.
  if (MATRIX_DEBUG)
  {
    cout << "rotate:" << endl;
    rotate.print();
  }
  // Return multiple rotations.
  return rotate;
}

/**
 * @brief Create a perspective matrix for the UI.
 * 
 * @param far the largest point of the perspective.
 * @param near the smallest point of the perspective.
 * @param degrees the fov.
 * @return Mat4 with the perspective adjusted for the UI.
 */
Mat4 Mat4::perspectiveProjection(float far, float near, float degrees)
{
  // Calculate the half angle tangent.
  float half_angle_tan = tan(Mat4::convertToRadians(degrees / 2.0f));
  // Calculate A with respect to half the angle and aspect ratio.
  float a = 1.0 / (half_angle_tan * (WINDOW_DIM_Y / WINDOW_DIM_X));
  // Calculate B with respect to half the angle.
  float b = 1.0 / half_angle_tan;
  // Calculate C with respect to near and far.
  float c = (-far-near) / (near-far);
  // Calculate C with respect to near and far.
  float d = (2.0 * far * near) / (near - far);
  // Set E to be positive.
  float e = 1.0;
  // Construct the array to convert to a matrix.
  float array[ROW_SIZE][COL_SIZE] = {
      {a, 0, 0, 0},
      {0, b, 0, 0},
      {0, 0, c, d},
      {0, 0, e, 0}};
  // Convert array to matrix.
  Mat4 perspectiveProjection = Mat4(array);
  // Print matrix if in debug mode.
  if (MATRIX_DEBUG)
  {
    cout << "perspectiveProjection" << endl;
    perspectiveProjection.print();
  }
  // Return perspective projection;
  return perspectiveProjection;
}
