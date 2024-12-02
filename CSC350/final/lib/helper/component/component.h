
/**
 * @file component.h
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Template for Component class.
 * @version 0.1
 * @date 2021-12-10
 * 
 * @copyright Copyright (c) 2021
 * 
 */
// Links to other files
#include "../shape/shape.h"
#include "../color/color.h"
#include "../mat4/mat4.h"
#include "../transformation/transformation.h"

// Headers for OpenGL on macOS
#define GL_SILENCE_DEPRECATION
#include <OpenGL/gl3.h>
#define __gl_h_
#include <OpenGL/glu.h>
#include <OpenGL/gl3ext.h>
#include <GLUT/glut.h>

// Built-in libraries
#include <vector>
#include <iostream>
#include <string>

using namespace std;

/**
 * @brief Component class used for hierarchical modeling.
 * 
 */
class Component
{
  private:
    // ID of the OpenGL program.
    GLuint programID;
    // Shae data to draw.
    Shape shape;
    // Color data to apply.
    Color color;
    // Component name. Used for debugging.
    string name;
    // Flag indiciating if component has a parent.
    bool hasParent;
    // Pointer to the parent.
    Component* parent;
    // Rotation matrix for the component.
    mat4 rotate;
    // Translation matrix for the component.
    mat4 translate;
    // Scale matrix for the component.
    mat4 scale;
    // List of children the component has.
    vector<Component *> children;
    
  public:

    /**
     * @brief Construct a new Component object
     * 
     * @param programID ID of the OpenGL program.
     * @param data Shae data to draw.
     * @param color Color data to apply.
     * @param nickname Component name. Used for debugging.
     */
    Component(GLuint programID, Shape data, Color color, string nickname);

    /**
     * @brief Links a child to a parent.
     * 
     * @param parent Pointer to the parent Component.
     * @param child Pointer to the child Component.
     */
    void addChild(Component *parent, Component *child);

    /**
     * @brief Set the Translation matrix for the Component.
     * 
     * @param x Units to shift by on the X-axis.
     * @param y Units to shift by on the Y-axis.
     * @param z Units to shift by on the Z-axis.
     */
    void setTranslate(float x, float y, float z);

    /**
     * @brief Set the Scale matrix for the Component.
     * 
     * @param x Units to scale by on the X-axis.
     * @param y Units to scale by on the Y-axis.
     * @param z Units to scale by on the Z-axis.
     */
    void setScale(float x, float y, float z);

    /**
     * @brief Set the Rotate matrix for the Component.
     * 
     * @param x Units to rotate by on the X-axis.
     * @param y Units to rotate by on the Y-axis.
     * @param z Units to rotate by on the Z-axis.
     */
    void setRotate(float x, float y, float z);

    /**
     * @brief Get the Rotation matrix for the Component.
     * 
     * @return Product of all parent rotations and the curent rotation.
     */
    mat4 getRotation(void);

    /**
     * @brief Get the Scale matrix for the Component.
     * 
     * @return Product of all parent scales and the curent scales. 
     */
    mat4 getScale(void);

    /**
     * @brief Get the net transformation matrix for the Component.
     * 
     * @return Product of all parent transformations and the curent transformation.
     */
    mat4 getTransformation(void);

    /**
     * @brief Loads the component into the GPU and then draws.
     * 
     */
    void display(void);
};