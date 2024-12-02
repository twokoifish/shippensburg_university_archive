#include "shape.h"

Shape::Shape(ShapeType type)
{
  vector<vec4> vertices;
  vector<vec4> normals;
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
    vertices = subdivide(vertices, 3);
    vertices = normalizeVertices(vertices);
    break;
  }
  normals = getSurfaceNormals(vertices);
  data = buildShape(vertices, normals);
}

vector<vec4> Shape::getCylinder(void)
{
  vector<vec4> square = {
      vec4(1, 0, 0),
      vec4(0, -1, 0),
      vec4(-1, 0, 0),
      vec4(0, 1, 0),
      vec4(1, 0, 0),
  };

  vector<vec4> n;
  for (int i = 0; i < 2; i++)
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

vector<vec4> Shape::subdivide(vector<vec4> vertices, int times)
{
  vector<vec4> subdividedVertices;

  for (int i = 0; i < vertices.size(); i++)
  {
    subdividedVertices.push_back(vertices[i]);
  }

  for (int i = 0; i < times; i++)
  {
    vector<vec4> temp;
    for (int j = 0; j < subdividedVertices.size(); j += 3)
    {
      vec4 a = subdividedVertices[j];
      vec4 b = subdividedVertices[j + 1];
      vec4 c = subdividedVertices[j + 2];

      vec4 ab = a | b; // a
      vec4 ac = a | c; // b
      vec4 bc = b | c; // c

      temp.push_back(a);
      temp.push_back(ab);
      temp.push_back(ac);

      temp.push_back(b);
      temp.push_back(bc);
      temp.push_back(ab);

      temp.push_back(c);
      temp.push_back(ac);
      temp.push_back(bc);

      temp.push_back(ab);
      temp.push_back(bc);
      temp.push_back(ac);
    }
    subdividedVertices.clear();

    for (int k = 0; k < temp.size(); k++)
    {
      subdividedVertices.push_back(temp[k]);
    }
    
  }
  return subdividedVertices;
}

vector<vec4> Shape::getSurfaceNormals(vector<vec4> vertices)
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

vector<float> Shape::buildShape(vector<vec4> vertices, vector<vec4> normals)
{
  vector<float> shape;
  for (int i = 0; i < vertices.size(); i++)
  {
    shape.insert(shape.end(), {
        vertices[i].x,
        vertices[i].y,
        vertices[i].z,
        vertices[i].w,
        normals[i].x,
        normals[i].y,
        normals[i].z,
        normals[i].w,
    });
  }
  return shape;
}

vector<float> Shape::getData(void)
{
  return data;
}