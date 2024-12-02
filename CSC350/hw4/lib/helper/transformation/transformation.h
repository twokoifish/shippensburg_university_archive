/**
 * @file transformation.h
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Template for a transformation getter class.
 * @version 0.1
 * @date 2021-11-16
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef TRANSFORMATION_H
#define TRANSFORMATION_H

#include <iostream>
#include <cmath>

#include "../mat4/mat4.h"

using namespace std;

class Transformation
{
  private:

  /**
   * @brief Returns degrees as radians.
   * 
   * @param degrees the angle in degrees.
   * @return degrees as radians. 
   */
  float convertToRadians(float degrees)
  {
    return degrees * (M_PI / 180.0);
  }

  public:
  /**
   * @brief Returns a perspective projection matrix with the given ranges.
   * 
   * @param near the nearest nxn window.
   * @param far the farthest mxm window.
   * @param degrees the fov of the window.
   * @return a perspective projection matrix with the given ranges.
   */
  static mat4 perspectiveProjection(float near, float far, float degrees);

  /**
   * @brief Returns a translation matrix with the given adjustments.
   * 
   * @param unitsX the units to translate by on the X axis.
   * @param unitsY the units to translate by on the Y axis.
   * @param unitsZ the units to translate by on the Z axis.
   * @return a translation matrix with the given adjustments. 
   */
  static mat4 translate(float unitsX, float unitsY, float unitsZ);

  /**
   * @brief Returns a rotation matrix with the given adjustments.
   * 
   * @param degreesX the degrees to rotate by around the X axis.
   * @param degreesY the degrees to rotate by around the Y axis. 
   * @param degreesZ the degrees to rotate by around the Z axis.
   * @return a rotation matrix with the given adjustments. 
   */
  static mat4 rotate(float degreesX, float degreesY, float degreesZ);

  /**
   * @brief Returns a scale matrix with the given adjustments.
   * 
   * @param unitsX the units to scale by on the X axis.
   * @param unitsY the units to scale by on the Y axis.
   * @param unitsZ the units to scale by on the Z axis.
   * @return a scale matrix with the given adjustments. 
   */
  static mat4 scale(float unitsX, float unitsY, float unitsZ);

};

#endif //TRANSFORMATION_H