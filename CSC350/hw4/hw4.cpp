#include "hw4.h"

GLuint programID;
GLuint vao;

int main(int argc, char **argv)
{
  glutInit(&argc, argv);
  glutInitDisplayModeAbstr(GLUT_DEPTH | GLUT_RGBA);
  glutInitWindowSize(1024, 1024);
  glutInitWindowPosition(0, 0);
  glutCreateWindow("Homework #4");
  glEnable(GL_DEPTH_TEST);

#ifndef __APPLE__
  GLenum glewStatus = glewInit();
  if (glewStatus != GLEW_OK)
  {
    exit(EXIT_FAILURE);
  }
#endif
  installShaders();
  //
  glGenVertexArrays(1, &vao);
  glBindVertexArray(vao);
  //
  glutDisplayFunc(display);
  glutMainLoop();
  return 0;
}

void glutInitDisplayModeAbstr(unsigned int mode)
{
#ifdef __APPLE__
  glutInitDisplayMode(mode | GLUT_3_2_CORE_PROFILE);
#else
  glutInitDisplayMode(mode);
#endif
}

void loadShader(GLuint shaderID, const char *shaderFileHandle)
{
  cout << "Loading " << shaderFileHandle << endl;
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
    cout << fileName << " not found" << endl;
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
    cout << "Error " << shaderID << endl;
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

void installShaders(void)
{
  programID = glCreateProgram();
  GLuint vertexShaderID = glCreateShader(GL_VERTEX_SHADER);
  loadShader(vertexShaderID, V_SHADER);
  GLuint fragmentShaderID = glCreateShader(GL_FRAGMENT_SHADER);
  loadShader(fragmentShaderID, F_SHADER);
  glLinkProgram(programID);
  checkForProgramError();
  glUseProgram(programID);
}

void display(void)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  vector<Color> c = {
      Color(0.97f, 0.84f, 0.09f, 1.00f),
      Color(0.87f, 0.65f, 0.03f, 1.00f),
      Color(0.56f, 0.40f, 0.06f, 1.00f),
      Color(0.53f, 0.50f, 0.34f, 1.00f),
      Color(0.09f, 0.56f, 0.97f, 1.00f),
      Color(0.06f, 0.37f, 0.81f, 1.00f),
      Color(0.00f, 0.25f, 0.62f, 1.00f),
      Color(0.94f, 0.94f, 0.94f, 1.00f),
      Color(0.81f, 0.81f, 0.81f, 1.00f),
      Color(0.59f, 0.62f, 0.62f, 1.00f),
      Color(0.28f, 0.31f, 0.34f, 1.00f),
      Color(0.21f, 0.25f, 0.31f, 1.00f),
      Color(0.18f, 0.21f, 0.25f, 1.00f),
      Color(0.15f, 0.18f, 0.21f, 1.00f),
      Color(1.00f, 0.89f, 0.54f, 1.00f),
  };

  vector<mat4> t = {
      Transformation::translate(10, 10, 25),
      Transformation::translate(-10, -10, 25),
  };
  vector<mat4> r = {
      Transformation::rotate(45, 45, 45),
      Transformation::rotate(0, -45 - 180, 0),
  };
  vector<mat4> s = {
      Transformation::scale(3, 3, 3),
      Transformation::scale(3, 3, 3),
  };

  Shape cyl = Shape(ST_CYLINDER);
  draw(cyl, t, r, s, c, 2);

  t = {
      Transformation::translate(0, 12, 30),
      Transformation::translate(7, 7, 30),
      Transformation::translate(12, 0, 30),
      Transformation::translate(7, -7, 30),
      Transformation::translate(0, -12, 30),
      Transformation::translate(-7, -7, 30),
      Transformation::translate(-12, 0, 30),
      Transformation::translate(-7, 7, 30),
  };
  r = {
      Transformation::rotate(10, 80, 10),
      Transformation::rotate(20, 70, 20),
      Transformation::rotate(30, 60, 30),
      Transformation::rotate(40, 50, 40),
      Transformation::rotate(50, 40, 50),
      Transformation::rotate(60, 30, 60),
      Transformation::rotate(70, 20, 70),
      Transformation::rotate(80, 10, 80),
  };
  s = {
      Transformation::scale(2.5, 2.5, 2.5),
      Transformation::scale(2.5, 2.5, 2.5),
      Transformation::scale(2.5, 2.5, 2.5),
      Transformation::scale(2.5, 2.5, 2.5),
      Transformation::scale(2.5, 2.5, 2.5),
      Transformation::scale(2.5, 2.5, 2.5),
      Transformation::scale(2.5, 2.5, 2.5),
      Transformation::scale(2.5, 2.5, 2.5),
  };

  Shape cube = Shape(ST_CUBE);
  draw(cube, t, r, s, c, 8);

  t = {
    Transformation::translate(-10, 10, 25),
    Transformation::translate(10, -10, 25),
  };

  r = {
    Transformation::rotate(15, 30, 45),
    Transformation::rotate(45, 30, 15),
  };

  s = {
    Transformation::scale(2.5, 2.5, 2.5),
    Transformation::scale(2.5, 2.5, 2.5),
  };

  Shape octa = Shape(ST_OCTAHEDRON);
  draw(octa, t, r, s, c, 2);

  t = {
    Transformation::translate(0, 0, 30),
  };

  r = {
    Transformation::rotate(10, 20, 30),
  };

  s = {
    Transformation::scale(10, 10, 10),
  };

  Shape sphere = Shape(ST_SPHERE);
  draw(sphere, t, r, s, c, 1);

  glFlush();
}

void enableAlign(GLuint buffer, GLuint id, GLint count, GLenum type, GLboolean normalized, GLint stride, GLint offset)
{
  glEnableVertexAttribArray(id);
  glBindBuffer(GL_ARRAY_BUFFER, buffer);
  glVertexAttribPointer(id, count, type, normalized, sizeof(GLfloat) * stride, BUFFER_OFFSET(sizeof(GLfloat) * offset));
}

void draw(Shape shape, vector<mat4> translate, vector<mat4> rotate, vector<mat4> scale, vector<Color> color, int count)
{
  // for (int i = 0; i < shape.getData().size(); i+=8)
  // {
  //   vector<float> data = shape.getData();
  //   cout << "point(" <<  data[i] << ", " << data[i+1] << ", " << data[i+2] << ", " << data[i+3] << ");" << endl;
  //   cout << "norma(" <<  data[i+4] << ", " << data[i+5] << ", " << data[i+6] << ", " << data[i+7] << ");" << endl;
  // }
  mat4 view = Transformation::perspectiveProjection(5, 100, 65);
  for (int i = 0; i < count; i++)
  {
    GLuint buffer = 0;
    glGenBuffers(1, &buffer);
    glBindBuffer(GL_ARRAY_BUFFER, buffer);
    const GLsizeiptr vSize = shape.getData().size() * sizeof(GLfloat);
    glBufferData(GL_ARRAY_BUFFER, vSize, &shape.getData().front(), GL_STATIC_DRAW);
    GLuint pos = 0;
    enableAlign(buffer, pos, 4, GL_FLOAT, GL_FALSE, 8, 0);
    GLuint normal = 1;
    enableAlign(buffer, normal, 4, GL_FLOAT, GL_FALSE, 8, 4);
    GLuint viewID = glGetUniformLocation(programID, "vProjection");
    glUniformMatrix4fv(viewID, 1, GL_TRUE, &view[0][0]);
    GLuint translateID = glGetUniformLocation(programID, "vTranslate");
    glUniformMatrix4fv(translateID, 1, GL_TRUE, &translate[i][0][0]);
    GLuint rotateID = glGetUniformLocation(programID, "vRotate");
    glUniformMatrix4fv(rotateID, 1, GL_TRUE, &rotate[i][0][0]);
    GLuint scaleID = glGetUniformLocation(programID, "vScale");
    glUniformMatrix4fv(scaleID, 1, GL_TRUE, &scale[i][0][0]);
    GLuint colorID = glGetUniformLocation(programID, "vColor");
    glUniform4fv(colorID, 1, &color[i].get().front());
    float vertices = shape.getData().size() / 2.0;
    glBindVertexArray(vao);
    glDrawArrays(GL_TRIANGLES, 0, vertices);
  }
}

