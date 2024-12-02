#ifndef SHAPE_H
#define SHAPE_H

#include <iostream>
#include <vector>
#include <cmath>
#include <array>
#include "../color/color.h"
#include "../../elementary/vec3/vec3.h"

using namespace std;

enum SHAPEenum
{
  SHAPE_CUBE,
  SHAPE_SPHERE,
  SHAPE_OCTAHEDRON,
};

class Shape
{
private:
  vector<float> vertices;
  vector<Vec3> normals;
public:
  Shape(vector<float> vertices, vector<Vec3> normals);
  vector<float> getVertices(void);
  vector<Vec3> getNormals(void);
  static vector<float> convertToFloat(vector<Vec3> vertices, Color c, vector<Vec3> normal);
  static Shape get(SHAPEenum shapeEnum, Color c);
  static vector<Vec3> fetchCubeData(void);
  static vector<Vec3> fetchOctahedronData(void);
  static vector<Vec3> subdivide(vector<Vec3> vertices, int times);
  static float calcMidpoint(float a, float b);
  static float calcNormal(float a, float b, float c);
  static float normalizePoint(float value, float distance, float normal);
  static vector<Vec3> normalize(vector<Vec3> data, float distance);
  static vector<Vec3> getNormals(vector<Vec3> vertices);
};

#endif