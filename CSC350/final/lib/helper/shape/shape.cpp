#include "shape.h"

/**
 * @brief Construct a new Shape object
 * 
 */
Shape::Shape(void)
{
  
}

/**
 * @brief Construct a new Shape object based on the type.
 * 
 * @param type Shape type to draw.
 */
Shape::Shape(ShapeType type)
{
  t = type;
  switch (type)
  {
  case ST_CUBE:
    vertices = getCube();
    break;
  case ST_OCTAHEDRON:
    vertices = getOctahedron();
    break;
  case ST_CYLINDER:
    vertices = getCylinder();
    break;
  case ST_SPHERE:
    vertices = getOctahedron();
    vertices = subdivide(vertices, 5);
    vertices = normalizeVertices(vertices);
    break;
  }
  normals = getSurfaceNormals(vertices);
  data = buildShape(vertices, normals);
}

/**
 * @brief Get the Type object
 * 
 * @return ShapeType 
 */
string Shape::getType(void)
{
  switch (t)
  {
  case ST_CUBE:
    return "CUBE";
  case ST_OCTAHEDRON:
    return "OCTAHEDRON";
  case ST_CYLINDER:
    return "CYLINDER";
  case ST_SPHERE:
    return "SPHERE";
  default:
    return "UNKNOWN";
  }
}


vector<vec4> Shape::getCylinder(void)
{
  vector<vec4> square;
  square.push_back(vec4(0.5, 0, 0));
  square.push_back(vec4(0, -0.5, 0));
  square.push_back(vec4(-0.5, 0, 0));
  square.push_back(vec4(0, 0.5, 0));
  square.push_back(vec4(0.5, 0, 0));
  

  vector<vec4> n;
  for (int i = 0; i < 5; i++)
  {
    n.clear();
    for (int j = 0; j < square.size() - 1; j++)
    {
      vec4 mid = square[j + 1] | square[j];
      n.push_back(square[j]);
      n.push_back(mid);
    }
    n.push_back(square[square.size() - 1]);
    square = n;
  }
  n = normalizeVertices(n);

  vector<vec4> top;
  vector<vec4> bottom;
  for (int i = 0; i < n.size(); i++)
  {
    top.push_back(vec4(n[i].x, n[i].y, 0.5));
    bottom.push_back(vec4(n[i].x, n[i].y, -0.5));
  }

  vector<vec4> vertices;
  for (int i = 0; i < n.size() - 1; i++)
  {
    vertices.push_back(vec4(n[i + 1].x, n[i + 1].y, 0.5));
    vertices.push_back(vec4(n[i].x, n[i].y, -0.5));
    vertices.push_back(vec4(n[i].x, n[i].y, 0.5));
    vertices.push_back(vec4(n[i + 1].x, n[i + 1].y, 0.5));
    vertices.push_back(vec4(n[i + 1].x, n[i + 1].y, -0.5));
    vertices.push_back(vec4(n[i].x, n[i].y, -0.5));
  }

  for (int i = 0; i < top.size() - 1; i++)
  {

    vertices.push_back(top[i + 1]);
    vertices.push_back(top[i]);
    vertices.push_back(vec4(0, 0, 0.5));
  }

  for (int i = 0; i < bottom.size() - 1; i++)
  {
    vertices.push_back(vec4(0, 0, -0.5));
    vertices.push_back(bottom[i]);

    vertices.push_back(bottom[i + 1]);
  }

  return vertices;
}

vector<vec4> Shape::getCube(void)
{
  // Create temporary buffer.
  vector<vec4> vertices;
  // Front. clock
  vertices.push_back(vec4(0.5, 0.5, -0.5));
  vertices.push_back(vec4(0.5, -0.5, -0.5));
  vertices.push_back(vec4(-0.5, -0.5, -0.5));
  vertices.push_back(vec4(-0.5, -0.5, -0.5));
  vertices.push_back(vec4(-0.5, 0.5, -0.5));
  vertices.push_back(vec4(0.5, 0.5, -0.5));

  // Back. counter
  vertices.push_back(vec4(0.5, 0.5, 0.5));
  vertices.push_back(vec4(-0.5, 0.5, 0.5));
  vertices.push_back(vec4(-0.5, -0.5, 0.5));
  vertices.push_back(vec4(-0.5, -0.5, 0.5));
  vertices.push_back(vec4(0.5, -0.5, 0.5));
  vertices.push_back(vec4(0.5, 0.5, 0.5));

  // Top.
  vertices.push_back(vec4(-0.5, 0.5, -0.5));
  vertices.push_back(vec4(-0.5, 0.5, 0.5));
  vertices.push_back(vec4(0.5, 0.5, 0.5));
  vertices.push_back(vec4(0.5, 0.5, 0.5));
  vertices.push_back(vec4(0.5, 0.5, -0.5));
  vertices.push_back(vec4(-0.5, 0.5, -0.5));

  // Bottom.
  vertices.push_back(vec4(-0.5, -0.5, -0.5));
  vertices.push_back(vec4(0.5, -0.5, -0.5));
  vertices.push_back(vec4(0.5, -0.5, 0.5));
  vertices.push_back(vec4(0.5, -0.5, 0.5));
  vertices.push_back(vec4(-0.5, -0.5, 0.5));
  vertices.push_back(vec4(-0.5, -0.5, -0.5));

  // Left.
  vertices.push_back(vec4(-0.5, 0.5, -0.5));
  vertices.push_back(vec4(-0.5, -0.5, -0.5));
  vertices.push_back(vec4(-0.5, -0.5, 0.5));
  vertices.push_back(vec4(-0.5, -0.5, 0.5));
  vertices.push_back(vec4(-0.5, 0.5, 0.5));
  vertices.push_back(vec4(-0.5, 0.5, -0.5));

  // Right.
  vertices.push_back(vec4(0.5, 0.5, 0.5));
  vertices.push_back(vec4(0.5, -0.5, 0.5));
  vertices.push_back(vec4(0.5, -0.5, -0.5));
  vertices.push_back(vec4(0.5, -0.5, -0.5));
  vertices.push_back(vec4(0.5, 0.5, -0.5));
  vertices.push_back(vec4(0.5, 0.5, 0.5));

  // Return vertices.
  return vertices;
}

vector<vec4> Shape::getOctahedron(void)
{
  // Create temporary buffer.
  vector<vec4> vertices;

  // Front, Top.
  vertices.push_back(vec4(0.0, 0.5, 0.0));
  vertices.push_back(vec4(0.5, 0.0, -0.5));
  vertices.push_back(vec4(-0.5, 0.0, -0.5));

  // Left, Top.
  vertices.push_back(vec4(0.0, 0.5, 0.0));
  vertices.push_back(vec4(-0.5, 0.0, -0.5));
  vertices.push_back(vec4(-0.5, 0.0, 0.5));

  // Right, Top.
  vertices.push_back(vec4(0.0, 0.5, 0.0));
  vertices.push_back(vec4(0.5, 0.0, 0.5));
  vertices.push_back(vec4(0.5, 0.0, -0.5));

  // Back, Top.
  vertices.push_back(vec4(0.0, 0.5, 0.0));
  vertices.push_back(vec4(-0.5, 0.0, 0.5));
  vertices.push_back(vec4(0.5, 0.0, 0.5));

  // Front, Bottom.
  vertices.push_back(vec4(0.0, -0.5, 0.0));
  vertices.push_back(vec4(-0.5, 0.0, -0.5));
  vertices.push_back(vec4(0.5, 0.0, -0.5));

  // Left, Bottom.
  vertices.push_back(vec4(0.0, -0.5, 0.0));
  vertices.push_back(vec4(-0.5, 0.0, 0.5));
  vertices.push_back(vec4(-0.5, 0.0, -0.5));

  // Right, Bottom.
  vertices.push_back(vec4(0.0, -0.5, 0.0));
  vertices.push_back(vec4(0.5, 0.0, -0.5));
  vertices.push_back(vec4(0.5, 0.0, 0.5));

  // Back, Bottom.
  vertices.push_back(vec4(0.0, -0.5, 0.0));
  vertices.push_back(vec4(0.5, 0.0, 0.5));
  vertices.push_back(vec4(-0.5, 0.0, 0.5));

  // Return vertices.
  return vertices;
}

vector<vec4> Shape::normalizeVertices(vector<vec4> vertices)
{
  vector<vec4> normalizedVertices;
  for (int i = 0; i < vertices.size(); i++)
  {
    normalizedVertices.push_back(!vertices[i]);
  }
  return normalizedVertices;
}

vector<vec4> Shape::subdivide(const vector<vec4> vertices, int times)
{
  vector<vec4> subdivided(vertices);
  for (int i = 0; i < times; i++)
  {
    vector<vec4> temporaryBuffer;
    for (int j = 0; j < subdivided.size(); j+=3)
    {
      vec4 a = subdivided[j];
      vec4 b = subdivided[j + 1];
      vec4 c = subdivided[j + 2];

      vec4 ab = a | b; // a
      vec4 ac = a | c; // b
      vec4 bc = b | c; // c

      temporaryBuffer.push_back(a);
      temporaryBuffer.push_back(ab);
      temporaryBuffer.push_back(ac);

      temporaryBuffer.push_back(b);
      temporaryBuffer.push_back(bc);
      temporaryBuffer.push_back(ab);

      temporaryBuffer.push_back(c);
      temporaryBuffer.push_back(ac);
      temporaryBuffer.push_back(bc);

      temporaryBuffer.push_back(ab);
      temporaryBuffer.push_back(bc);
      temporaryBuffer.push_back(ac);
    }
    subdivided = temporaryBuffer;
    temporaryBuffer.clear();
  }
  return subdivided;
}

vector<vec4> Shape::getSurfaceNormals(const vector<vec4> vertices)
{
  vector<vec4> surfaceNormals;
  for (int i = 0; i < vertices.size(); i += 3)
  {
    vec4 a = vertices[i];
    vec4 b = vertices[i + 1];
    vec4 c = vertices[i + 2];
    vec4 normal = (b - a) % (c - a);
    for (int j = 0; j < 3; j++)
    {
      surfaceNormals.push_back(normal);
    }
  }
  return surfaceNormals;
}

vector<float> Shape::buildShape(const vector<vec4> vertices, const vector<vec4> normals)
{
  vector<float> shape;
  for (int i = 0; i < vertices.size(); i++)
  {
    if (isnan(vertices[i].x) || isnan(vertices[i].y) || isnan(vertices[i].z) || isnan(vertices[i].w))
    {
      cout << "Culprit" << endl;
    }
    shape.push_back(vertices[i].x);
    shape.push_back(vertices[i].y);
    shape.push_back(vertices[i].z);
    shape.push_back(vertices[i].w);
    shape.push_back(normals[i].x);
    shape.push_back(normals[i].y);
    shape.push_back(normals[i].z);
    shape.push_back(normals[i].w);
  }
  return shape;
}

vector<float> Shape::getData(void)
{
  return data;
}