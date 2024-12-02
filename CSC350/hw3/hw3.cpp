#include "hw3.h"

GLuint programID;

int main(int argc, char **argv)
{
  initGLUT(argc, argv);
  glutDisplayFunc(displayGL);
  glutMainLoop();
  return 0;
}

void initGLUT(int argc, char **argv)
{
  glutInit(&argc, argv);
  glutInitDisplayModeAbstr(GLUT_DEPTH | GLUT_RGB);
  glutInitWindow();
  glEnable(GL_DEPTH_TEST | GL_CULL_FACE);
#ifndef __APPLE__
  glewInitAbstr();
#endif
  installShaders();
  GLuint vao = initVAO();
}

void glutInitDisplayModeAbstr(unsigned int mode)
{
#ifdef __APPLE__
  glutInitDisplayMode(mode | GLUT_3_2_CORE_PROFILE);
#else
  glutInitDisplayMode(mode);
#endif
}

void glutInitWindow(void)
{
  glutInitWindowSize(WINDOW_DIM_X, WINDOW_DIM_Y);
  glutInitWindowPosition(WINDOW_POS_X, WINDOW_POS_Y);
  glutCreateWindow(WINDOW_TITLE);
}

void glewInitAbstr(void)
{
#ifndef __APPLE__
  GLenum glewStatus = glewInit();
  if (glewStatus != GLEW_OK)
  {
    exit(EXIT_FAILURE);
  }
#endif
}

GLuint initVAO(void)
{
  GLuint vaoID = 0;
  glGenVertexArrays(VAO_COUNT, &vaoID);
  glBindVertexArray(vaoID);
  return vaoID;
}

GLuint initBuffer(vector<GLfloat> vertices)
{
  GLuint bufferID = 0;
  glGenBuffers(BUFFER_COUNT, &bufferID);
  glBindBuffer(GL_ARRAY_BUFFER, bufferID);
  const GLsizeiptr vSize = vertices.size() * sizeof(GLfloat);
  glBufferData(GL_ARRAY_BUFFER, vSize, &vertices.front(), GL_STATIC_DRAW);
  return bufferID;
}

void alignAttrib(GLuint id, GLint count, GLenum type, GLboolean isNormalized, GLint stride, GLint offset)
{
  const GLsizei attribStride = sizeof(GLfloat) * stride;
  const GLsizei attribOffset = sizeof(GLfloat) * offset;
  glEnableVertexAttribArray(id);
  glVertexAttribPointer(id, count, type, isNormalized, attribStride, BUFFER_OFFSET(attribOffset));
}

void installShaders(void)
{
  programID = glCreateProgram();
  GLuint vertexShaderID = glCreateShader(GL_VERTEX_SHADER);
  cout << VERTEX_SHADER_FH << endl;
  loadShader(vertexShaderID, VERTEX_SHADER_FH);
  GLuint fragmentShaderID = glCreateShader(GL_FRAGMENT_SHADER);
  cout << FRAGMENT_SHADER_FH << endl;
  loadShader(fragmentShaderID, FRAGMENT_SHADER_FH);
  glLinkProgram(programID);
  checkForProgramError();
  glUseProgram(programID);
}

void loadShader(GLuint shaderID, const char *shaderFileHandle)
{
  const char *adapter[1];
  adapter[0] = readShader(shaderFileHandle);
  glShaderSource(shaderID, 1, adapter, 0);
  glCompileShader(shaderID);
  checkForShaderError(shaderID);
  glAttachShader(programID, shaderID);
}

char *readShader(const char *fileName)
{
  FILE *fp = fopen(fileName, "r");
  if (fp == NULL)
  {
    exit(EXIT_FAILURE);
  }
  fseek(fp, 0L, SEEK_END);
  int res = ftell(fp);
  fseek(fp, 0, SEEK_SET);
  char *data = (char *)calloc(res + 1, sizeof(char));
  fread(data, sizeof(char), res, fp);
  fclose(fp);
  return data;
}

void checkForShaderError(GLuint shaderID)
{
  GLint compileStatus;
  glGetShaderiv(shaderID, GL_COMPILE_STATUS, &compileStatus);
  if (compileStatus != GL_TRUE)
  {
    GLint maxLength;
    glGetShaderiv(shaderID, GL_INFO_LOG_LENGTH, &maxLength);
    GLchar *errorLog = new GLchar[maxLength];
    GLsizei bufferSize;
    glGetShaderInfoLog(shaderID, maxLength, &bufferSize, errorLog);
    cout << errorLog << endl;
    delete[] errorLog;
    exit(EXIT_FAILURE);
  }
}

void checkForProgramError(void)
{
  GLint linkStatus;
  glGetProgramiv(programID, GL_LINK_STATUS, &linkStatus);
  if (linkStatus != GL_TRUE)
  {
    GLint maxLength;
    glGetProgramiv(programID, GL_INFO_LOG_LENGTH, &maxLength);
    GLchar *errorLog = new GLchar[maxLength];
    GLsizei bufferSize;
    glGetProgramInfoLog(programID, maxLength, &bufferSize, errorLog);
    cout << errorLog << endl;
    delete[] errorLog;
    exit(EXIT_FAILURE);
  }
}

int calcNumberOfVerticies(vector<GLfloat> vertices)
{
  int totalEntries = vertices.size();
  int colorEntries = (totalEntries / 7) * 4;
  totalEntries -= colorEntries;
  totalEntries /= 3;
  return totalEntries;
}

Shape drawSphere(Color c)
{
  Shape sphere = Shape::get(SHAPE_SPHERE, c);
  GLuint buffer = initBuffer(sphere.getVertices());
  const GLuint vPosition = 0;
  alignAttrib(vPosition, 3, GL_FLOAT, GL_FALSE, 10, 0);
  const GLuint vColor = 1;
  alignAttrib(vColor, 4, GL_FLOAT, GL_FALSE, 10, 3);
  const GLuint vNormal = 2;
  alignAttrib(vNormal, 3, GL_FLOAT, GL_FALSE, 10, 7);
  return sphere;
}

Shape drawCube(Color c)
{
  Shape cube = Shape::get(SHAPE_CUBE, c);
  GLuint buffer = initBuffer(cube.getVertices());
  const GLuint vPosition = 0;
  alignAttrib(vPosition, 3, GL_FLOAT, GL_FALSE, 10, 0);
  const GLuint vColor = 1;
  alignAttrib(vColor, 4, GL_FLOAT, GL_FALSE, 10, 3);
  const GLuint vNormal = 2;
  alignAttrib(vNormal, 3, GL_FLOAT, GL_FALSE, 10, 7);
  return cube;
  
}

void displayGL(void)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  vector<Color> colors = ColorManager::getPalette();
  for (int i = 0; i < colors.size(); i++)
  {
    Shape cube = drawCube(colors[i]);
    Mat4 projection = Mat4::perspectiveProjection(100.0, 5.0, 65.0);
    Mat4 translate = Mat4::translate((i * 0.5) - 4.0, (i * 0.50) - 4.0, 8);
    Mat4 scale = Mat4::scaleUniform(0.3);
    Mat4 rotate = Mat4::rotate(i * 12, i * 12 - i, i + 12);
    GLuint rotateID = glGetUniformLocation(programID, "rotationMatrix");
    glUniformMatrix4fv(rotateID, 1, GL_TRUE, &rotate[0][0]);
    Mat4 transformation = projection * translate * scale * rotate;
    GLuint matrixID = glGetUniformLocation(programID, "transformedMatrix");
    glUniformMatrix4fv(matrixID, 1, GL_TRUE, &transformation[0][0]);
    glDrawArrays(GL_TRIANGLES, 0, calcNumberOfVerticies(cube.getVertices()));
  }

  for (int i = 0; i < 2; i++)
  {
    Shape sphere = drawSphere(colors[i + 3]);
    Mat4 projection = Mat4::perspectiveProjection(100.0, 5.0, 65.0);
    Mat4 translate;
    if (i)
    {
      translate = Mat4::translate(2, -2, 5);
    }
    else
    {
      translate = Mat4::translate(-2, 2, 14);
    }
    Mat4 scale = Mat4::scaleUniform(3);
    Mat4 rotate = Mat4::rotate(i * 15, i * 15 - i, i + 15);
    GLuint rotateID = glGetUniformLocation(programID, "rotationMatrix");
    glUniformMatrix4fv(rotateID, 1, GL_TRUE, &rotate[0][0]);
    Mat4 transformation = projection * translate * scale * rotate;
    GLuint matrixID = glGetUniformLocation(programID, "transformedMatrix");
    glUniformMatrix4fv(matrixID, 1, GL_TRUE, &transformation[0][0]);
    glDrawArrays(GL_TRIANGLES, 0, calcNumberOfVerticies(sphere.getVertices()));
  }
  glFlush();
}
