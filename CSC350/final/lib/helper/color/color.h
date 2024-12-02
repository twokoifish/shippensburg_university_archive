/**
 * @file color.h
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Template for Color class.
 * @version 0.1
 * @date 2021-12-10
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef COLOR_H
#define COLOR_H

// Built-in libraries
#include <iostream>
#include <vector>

using namespace std;

/**
 * @brief Color class used for fragment shader.
 * 
 */
class Color
{
  private:
    // Array version of color data.
    vector<float> data;

  public:

    /**
     * @brief Construct a new Color object
     * 
     */
    Color(void);

    /**
     * @brief Construct a new Color object
     * 
     * @param redVal Float value for red.
     * @param greenVal Float value for green.
     * @param blueVal Float value for blue.
     */
    Color(float redVal, float greenVal, float blueVal);

    /**
     * @brief Construct a new Color object
     * 
     * @param redVal Float value for red.
     * @param greenVal Float value for green.
     * @param blueVal Float value for blue.
     * @param alphaVal Float value for alpha.
     */
    Color(float redVal, float greenVal, float blueVal, float alphaVal);

    /**
     * @brief Returns the Color data as an array.
     * 
     * @return Array of Color data.
     */
    vector<float> get(void);
};

#endif // COLOR_H