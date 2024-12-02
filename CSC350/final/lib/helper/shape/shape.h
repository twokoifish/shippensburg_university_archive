/**
 * @file shape.h
 * @author your name (you@domain.com)
 * @brief 
 * @version 0.1
 * @date 2021-11-16
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef SHAPE_H
#define SHAPE_H

#include "../vec4/vec4.h"
#include "../color/color.h"
#include <vector>
#include <string>

using namespace std;

/**
 * @brief enum for shapes that can be drawn.
 * 
 */
enum ShapeType
{
  ST_CUBE,
  ST_OCTAHEDRON,
  ST_SPHERE,
  ST_CYLINDER,
};

/**
 * @brief Class for drawing shapes.
 * 
 */
class Shape
{
private:
  vector<float> data;
  vector<vec4> vertices;
  vector<vec4> normals;
  ShapeType t;

public:

  /**
   * @brief Construct a new Shape object
   * 
   */
  Shape(void);

  /**
   * @brief Construct a new Shape object based on the type.
   * 
   * @param type Shape type to draw.
   */
  Shape(ShapeType type);

  /**
   * @brief Get the Type object
   * 
   * @return ShapeType 
   */
  string getType(void);

  /**
   * @brief Get the Cylinder object
   * 
   * @return vector<vec4> 
   */
  vector<vec4> getCylinder(void);

  /**
   * @brief Get the Cube object
   * 
   * @return vector<vec4> 
   */
  vector<vec4> getCube(void);

  /**
   * @brief Get the Octahedron object
   * 
   * @return vector<vec4> 
   */
  vector<vec4> getOctahedron(void);

  /**
   * @brief 
   * 
   * @param vertices 
   * @return vector<vec4> 
   */
  vector<vec4> normalizeVertices(vector<vec4> vertices);

  /**
   * @brief 
   * 
   * @param vertices 
   * @param times 
   * @return vector<vec4> 
   */
  vector<vec4> subdivide(vector<vec4> vertices, int times);

  /**
   * @brief Get the Surface Normals object
   * 
   * @param vertices 
   * @return vector<vec4> 
   */
  vector<vec4> getSurfaceNormals(vector<vec4> vertices);

  /**
   * @brief 
   * 
   * @param vertices 
   * @param normals 
   * @return vector<float> 
   */
  vector<float> buildShape(vector<vec4> vertices, vector<vec4> normals);

  /**
   * @brief Get the Data object
   * 
   * @return vector<float> 
   */
  vector<float> getData(void);
};

#endif //SHAPE_H