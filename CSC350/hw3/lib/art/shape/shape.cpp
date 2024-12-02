#include "shape.h"
Shape::Shape(vector<float> vertices, vector<Vec3> normals)
{
  this->vertices = vertices;
  this->normals = normals;
}

vector<float> Shape::getVertices(void)
{
  return vertices;
}

vector<Vec3> Shape::getNormals(void)
{
  return normals;
}

vector<float> Shape::convertToFloat(vector<Vec3> vertices, Color c, vector<Vec3> normal)
{
  vector<float> shape;
  int normalV = 0;
  for (int i = 0; i < vertices.size(); i++)
  {
    if (normalV % 3 == 0)
    {
      normalV++;
    }
    shape.push_back(vertices[i].x);
    shape.push_back(vertices[i].y);
    shape.push_back(vertices[i].z);
    shape.push_back(c.red);
    shape.push_back(c.green);
    shape.push_back(c.blue);
    shape.push_back(c.alpha);
    shape.push_back(normal[normalV].x);
    shape.push_back(normal[normalV].y);
    shape.push_back(normal[normalV].z);
  }
  return shape;
}

Shape Shape::get(SHAPEenum shapeEnum, Color c)
{
  vector<Vec3> shape;
  switch (shapeEnum)
  {
  case SHAPE_CUBE:
    shape = Shape::fetchCubeData();
    break;
  case SHAPE_SPHERE:
    shape = Shape::fetchOctahedronData();
    shape = Shape::subdivide(shape, 3);
    shape = Shape::normalize(shape, 0.5);
    break;
  case SHAPE_OCTAHEDRON:
    shape = Shape::fetchOctahedronData();
    break;
  }
  vector<Vec3> normal = Shape::getNormals(shape);
  Shape s = Shape(Shape::convertToFloat(shape, c, normal), normal);
  return s;
}

vector<Vec3> Shape::fetchCubeData(void)
{
  vector<Vec3> cube;
  cube.push_back(Vec3(-0.5, 0.5, 0.5));
  cube.push_back(Vec3(-0.5, -0.5, 0.5));
  cube.push_back(Vec3(0.5, 0.5, 0.5));
  cube.push_back(Vec3(0.5, -0.5, 0.5));
  cube.push_back(Vec3(-0.5, -0.5, 0.5));
  cube.push_back(Vec3(0.5, 0.5, 0.5));
  cube.push_back(Vec3(-0.5, 0.5, -0.5));
  cube.push_back(Vec3(-0.5, -0.5, -0.5));
  cube.push_back(Vec3(0.5, 0.5, -0.5));
  cube.push_back(Vec3(0.5, -0.5, -0.5));
  cube.push_back(Vec3(-0.5, -0.5, -0.5));
  cube.push_back(Vec3(0.5, 0.5, -0.5));
  cube.push_back(Vec3(-0.5, 0.5, 0.5));
  cube.push_back(Vec3(-0.5, -0.5, 0.5));
  cube.push_back(Vec3(-0.5, 0.5, -0.5));
  cube.push_back(Vec3(-0.5, 0.5, 0.5));
  cube.push_back(Vec3(-0.5, -0.5, -0.5));
  cube.push_back(Vec3(-0.5, -0.5, 0.5));
  cube.push_back(Vec3(0.5, 0.5, 0.5));
  cube.push_back(Vec3(0.5, -0.5, 0.5));
  cube.push_back(Vec3(0.5, 0.5, -0.5));
  cube.push_back(Vec3(0.5, 0.5, -0.5));
  cube.push_back(Vec3(0.5, -0.5, -0.5));
  cube.push_back(Vec3(0.5, 0.5, 0.5));
  cube.push_back(Vec3(-0.5, 0.5, 0.5));
  cube.push_back(Vec3(0.5, 0.5, 0.5));
  cube.push_back(Vec3(0.5, 0.5, -0.5));
  cube.push_back(Vec3(-0.5, 0.5, -0.5));
  cube.push_back(Vec3(0.5, 0.5, -0.5));
  cube.push_back(Vec3(-0.5, 0.5, 0.5));
  cube.push_back(Vec3(-0.5, -0.5, 0.5));
  cube.push_back(Vec3(0.5, -0.5, 0.5));
  cube.push_back(Vec3(0.5, -0.5, -0.5));
  cube.push_back(Vec3(-0.5, -0.5, -0.5));
  cube.push_back(Vec3(0.5, -0.5, -0.5));
  cube.push_back(Vec3(-0.5, -0.5, 0.5));
  return cube;
}

vector<Vec3> Shape::fetchOctahedronData(void)
{
  vector<Vec3> octahedron;
  octahedron.push_back(Vec3(-0.5, 0, 0.5));
  octahedron.push_back(Vec3(0.5, 0, 0.5));
  octahedron.push_back(Vec3(0, 0.5, 0));
  octahedron.push_back(Vec3(-0.5, 0, 0.5));
  octahedron.push_back(Vec3(-0.5, 0, -0.5));
  octahedron.push_back(Vec3(0, 0.5, 0));
  octahedron.push_back(Vec3(-0.5, 0, -0.5));
  octahedron.push_back(Vec3(0.5, 0, -0.5));
  octahedron.push_back(Vec3(0, 0.5, 0));
  octahedron.push_back(Vec3(0.5, 0, 0.5));
  octahedron.push_back(Vec3(0.5, 0, -0.5));
  octahedron.push_back(Vec3(0, 0.5, 0));
  octahedron.push_back(Vec3(-0.5, 0, 0.5));
  octahedron.push_back(Vec3(0.5, 0, 0.5));
  octahedron.push_back(Vec3(0, -0.5, 0));
  octahedron.push_back(Vec3(-0.5, 0, 0.5));
  octahedron.push_back(Vec3(-0.5, 0, -0.5));
  octahedron.push_back(Vec3(0, -0.5, 0));
  octahedron.push_back(Vec3(-0.5, 0, -0.5));
  octahedron.push_back(Vec3(0.5, 0, -0.5));
  octahedron.push_back(Vec3(0, -0.5, 0));
  octahedron.push_back(Vec3(0.5, 0, 0.5));
  octahedron.push_back(Vec3(0.5, 0, -0.5));
  octahedron.push_back(Vec3(0, -0.5, 0));
  return octahedron;
}

vector<Vec3> Shape::subdivide(vector<Vec3> vertices, int times)
{
  if (!times)
  {
    return Shape::fetchOctahedronData();
  }
  else
  {
    vector<Vec3> subdivided;
    for (int i = 0; i < times; i++)
    {
      for (int j = 0; j < vertices.size(); j += 3)
      {
        Vec3 a = vertices[j];
        Vec3 b = vertices[j + 1];
        Vec3 c = vertices[j + 2];

        float xAB = Shape::calcMidpoint(a.x, b.x);
        float yAB = Shape::calcMidpoint(a.y, b.y);
        float zAB = Shape::calcMidpoint(a.z, b.z);

        float xAC = Shape::calcMidpoint(a.x, c.x);
        float yAC = Shape::calcMidpoint(a.y, c.y);
        float zAC = Shape::calcMidpoint(a.z, c.z);

        float xBC = Shape::calcMidpoint(b.x, c.x);
        float yBC = Shape::calcMidpoint(b.y, c.y);
        float zBC = Shape::calcMidpoint(b.z, c.z);

        subdivided.push_back(a);
        subdivided.push_back(Vec3(xAB, yAB, zAB));
        subdivided.push_back(Vec3(xAC, yAC, zAC));

        subdivided.push_back(Vec3(xAB, yAB, zAB));
        subdivided.push_back(b);
        subdivided.push_back(Vec3(xBC, yBC, zBC));

        subdivided.push_back(Vec3(xAC, yAC, zAC));
        subdivided.push_back(Vec3(xBC, yBC, zBC));
        subdivided.push_back(c);
        
        subdivided.push_back(Vec3(xAC, yAC, zAC));
        subdivided.push_back(Vec3(xAB, yAB, zAB));
        subdivided.push_back(Vec3(xBC, yBC, zBC));
      }
      vertices = subdivided;
    }
    return subdivided;
  }
}

float Shape::calcMidpoint(float a, float b)
{
  return (a + b) / 2.0;
}

float Shape::calcNormal(float a, float b, float c)
{
  return sqrt(pow(a, 2) + pow(b, 2) + pow(c, 2));
}

float Shape::normalizePoint(float value, float distance, float normal)
{
  return (value * distance) / normal;
}

vector<Vec3> Shape::normalize(vector<Vec3> data, float distance)
{
  vector<Vec3> shape;
  for (int i = 0; i < data.size(); i++)
  {
    float normal = Shape::calcNormal(data[i].x, data[i].y, data[i].z);
    float x = Shape::normalizePoint(data[i].x, distance, normal);
    float y = Shape::normalizePoint(data[i].y, distance, normal);
    float z = Shape::normalizePoint(data[i].z, distance, normal);
    shape.push_back(Vec3(x, y, z));
  }
  return shape;
}

vector<Vec3> Shape::getNormals(vector<Vec3> vertices)
{
  vector<Vec3> normals;
  for (int i = 0; i < vertices.size(); i += 3)
  {
    Vec3 U = vertices[i + 1] - vertices[i];
    Vec3 V = vertices[i + 2] - vertices[i];
    Vec3 normal = U.cross(V);
    for (int i = 0; i < 3; i++)
    {
      normals.push_back(normal);
    }
  }
  return normals;
}