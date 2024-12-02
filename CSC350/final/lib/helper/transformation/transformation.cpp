/**
 * @file transformation.cpp
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Implementation of a transformation getter class.
 * @version 0.1
 * @date 2021-11-16
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "transformation.h"

/**
 * @brief Returns a perspective projection matrix with the given ranges.
 * 
 * @param near the nearest nxn window.
 * @param far the farthest mxm window.
 * @param degrees the fov of the window.
 * @return a perspective projection matrix with the given ranges.
 */
mat4 Transformation::perspectiveProjection(float near, float far, float degrees)
{
  float rad = (degrees / 2.0) * (M_PI / 180.0);

  float half_a = tan(rad);

  float a = 1.0 / half_a;
  float b = a;
  float c = (-far - near) / (near - far);
  float d = (2.0 * far * near) / (near - far);
  float e = 1.0;

  float p[4][4] = {
      {a, 0, 0, 0},
      {0, b, 0, 0},
      {0, 0, c, d},
      {0, 0, e, 0},
  };

  return mat4(p);
}

/**
 * @brief Returns an identity matrix.
 * 
 * @return An identity matrix. 
 */
mat4 Transformation::identity(void)
{
  float data[4][4] = {
      {1, 0, 0, 0},
      {0, 1, 0, 0},
      {0, 0, 1, 0},
      {0, 0, 0, 1},
  };
  return mat4(data);
}

/**
 * @brief Returns a translation matrix with the given adjustments.
 * 
 * @param unitsX the units to translate by on the X axis.
 * @param unitsY the units to translate by on the Y axis.
 * @param unitsZ the units to translate by on the Z axis.
 * @return a translation matrix with the given adjustments. 
 */
mat4 Transformation::translate(float unitsX, float unitsY, float unitsZ)
{
  float data[4][4] = {
      {1, 0, 0, 0},
      {0, 1, 0, 0},
      {0, 0, 1, 0},
      {0, 0, 0, 1},
  };
  mat4 identity = mat4(data);
  if (unitsX != 0)
  {
    float x[4][4] = {
        {1, 0, 0, unitsX},
        {0, 1, 0, 0},
        {0, 0, 1, 0},
        {0, 0, 0, 1},
    };

    identity = identity * mat4(x);
  }
  if (unitsY != 0)
  {
    float y[4][4] = {
        {1, 0, 0, 0},
        {0, 1, 0, unitsY},
        {0, 0, 1, 0},
        {0, 0, 0, 1},
    };
    identity = identity * mat4(y);
  }
  if (unitsZ != 0)
  {
    float z[4][4] = {
        {1, 0, 0, 0},
        {0, 1, 0, 0},
        {0, 0, 1, unitsZ},
        {0, 0, 0, 1},
    };
    identity = identity * mat4(z);
  }
  return identity;
}

/**
 * @brief Returns a rotation matrix with the given adjustments.
 * 
 * @param degreesX the degrees to rotate by around the X axis.
 * @param degreesY the degrees to rotate by around the Y axis. 
 * @param degreesZ the degrees to rotate by around the Z axis.
 * @return a rotation matrix with the given adjustments. 
 */
mat4 Transformation::rotate(float degreesX, float degreesY, float degreesZ)
{
  float data[4][4] = {
      {1, 0, 0, 0},
      {0, 1, 0, 0},
      {0, 0, 1, 0},
      {0, 0, 0, 1},
  };
  mat4 identity = mat4(data);
  if (degreesX != 0)
  {
    float radX = degreesX * (M_PI / 180.0);
    float x[4][4] = {
        {1, 0, 0, 0},
        {0, cos(radX), -sin(radX), 0},
        {0, sin(radX), cos(radX), 0},
        {0, 0, 0, 1},
    };

    identity = identity * mat4(x);
  }
  if (degreesY != 0)
  {
    float radY = degreesY * (M_PI / 180.0);
    float y[4][4] = {
        {cos(radY), 0, sin(radY), 0},
        {0, 1, 0, 0},
        {-sin(radY), 0, cos(radY), 0},
        {0, 0, 0, 1},
    };
    identity = identity * mat4(y);
  }
  if (degreesZ != 0)
  {
    float radZ = degreesZ * (M_PI / 180.0);
    float z[4][4] = {
        {cos(radZ), -sin(radZ), 0, 0},
        {sin(radZ), cos(radZ), 0, 0},
        {0, 0, 1, 0},
        {0, 0, 0, 1},
    };
    identity = identity * mat4(z);
  }
  return identity;
}

/**
 * @brief Returns a scale matrix with the given adjustments.
 * 
 * @param unitsX the units to scale by on the X axis.
 * @param unitsY the units to scale by on the Y axis.
 * @param unitsZ the units to scale by on the Z axis.
 * @return a scale matrix with the given adjustments. 
 */
mat4 Transformation::scale(float unitsX, float unitsY, float unitsZ)
{
  float data[4][4] = {
      {1, 0, 0, 0},
      {0, 1, 0, 0},
      {0, 0, 1, 0},
      {0, 0, 0, 1},
  };
  mat4 identity = mat4(data);
  if (unitsX != 0)
  {
    float x[4][4] = {
        {unitsX, 0, 0, 0},
        {0, 1, 0, 0},
        {0, 0, 1, 0},
        {0, 0, 0, 1},
    };

    identity = identity * mat4(x);
  }
  if (unitsY != 0)
  {
    float y[4][4] = {
        {1, 0, 0, 0},
        {0, unitsY, 0, 0},
        {0, 0, 1, 0},
        {0, 0, 0, 1},
    };
    identity = identity * mat4(y);
  }
  if (unitsZ != 0)
  {
    float z[4][4] = {
        {1, 0, 0, 0},
        {0, 1, 0, 0},
        {0, 0, unitsZ, 0},
        {0, 0, 0, 1},
    };
    identity = identity * mat4(z);
  }
  return identity;
}