#ifndef HW3_H
#define HW3_H

#include <iostream>
#include <vector>

#ifdef __APPLE__
#define GL_SILENCE_DEPRECATION
#include <OpenGL/gl3.h>
#define __gl_h_
#include <OpenGL/glu.h>
#include <OpenGL/gl3ext.h>
#include <GLUT/glut.h>
#else
#include <GL/glew.h>
#include <GL/glut.h>
#endif //__APPLE__

#include "lib/art/art.h"
#include "lib/elementary/mat4/mat4.h"

#define HW3_DEBUG_MODE 1
#define WINDOW_DIM_X 512
#define WINDOW_DIM_Y 512
#define WINDOW_POS_X 0
#define WINDOW_POS_Y 0
#define WINDOW_TITLE "Homework #3"
#define COLOR_PALETTE_SIZE 16
#define VAO_COUNT 1
#define BUFFER_COUNT 1
#define BUFFER_OFFSET(i) ((char *)NULL + (i))
#define VERTEX_SHADER_FH "lib/assets/shaders/VertexShader.glsl"
#define FRAGMENT_SHADER_FH "lib/assets/shaders/FragmentShader.glsl"

using namespace std;

class GLObject
{
  private:
  GLuint vaoID;
  vector<float> bufferData;

  public:
  GLObject(GLuint vaoID, vector<float> bufferData);
  GLuint getVaoID(void);
  vector<float> getBufferData(void);
};

void initGLUT(int argc, char **argv);
void glutInitDisplayModeAbstr(unsigned int mode);
void glutInitWindow(void);
void printDebugInfo(void);
void buildEnvironment();
void loadBuffer(void);
void glewInitAbstr(void);
GLuint initVAO(void);
GLuint initBuffer(vector<GLfloat> vertices);
void alignAttrib(GLuint id, GLint count, GLenum type, GLboolean isNormalized, GLint stride, GLint offset);
void installShaders(void);
void loadShader(GLuint shaderID, const char *shaderFileHandle);
char *readShader(const char *fileName);
void checkForShaderError(GLuint shaderID);
void checkForProgramError();
int calcNumberOfVerticies(vector<GLfloat> vertices);
void displayGL(void);

#endif //HW3_H

