/**
 * @file color.cpp
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Implementation for Color class.
 * @version 0.1
 * @date 2021-12-10
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "color.h"

/**
 * @brief Construct a new Color object
 * 
 */
Color::Color(void)
{
  data.push_back(1.0f);
  data.push_back(1.0f);
  data.push_back(1.0f);
  data.push_back(1.0f);
}

/**
 * @brief Construct a new Color object
 * 
 * @param redVal Float value for red.
 * @param greenVal Float value for green.
 * @param blueVal Float value for blue.
 */
Color::Color(float redVal, float greenVal, float blueVal)
{
  data.push_back(redVal);
  data.push_back(greenVal);
  data.push_back(blueVal);
  data.push_back(1.0f);
}

/**
 * @brief Construct a new Color object
 * 
 * @param redVal Float value for red.
 * @param greenVal Float value for green.
 * @param blueVal Float value for blue.
 * @param alphaVal Float value for alpha.
 */
Color::Color(float redVal, float greenVal, float blueVal, float alphaVal)
{
  data.push_back(redVal);
  data.push_back(greenVal);
  data.push_back(blueVal);
  data.push_back(alphaVal);
}

/**
 * @brief Returns the Color data as an array.
 * 
 * @return Array of Color data.
 */
vector<float> Color::get(void)
{
  return data;
}