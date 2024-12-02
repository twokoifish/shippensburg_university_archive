#ifndef HW4_H
#define HW4_H

#ifdef __APPLE__
#define GL_SILENCE_DEPRECATION
#include <OpenGL/gl3.h>
#define __gl_h_
#include <OpenGL/glu.h>
#include <OpenGL/gl3ext.h>
#include <GLUT/glut.h>
#define MACOSX_GIDM_PARAM GLUT_3_2_CORE_PROFILE
#else
#include <GL/glew.h>
#include <GL/glut.h>
#endif //__APPLE__

#define V_SHADER "lib/assets/shaders/vertex_shader.vs"
#define F_SHADER "lib/assets/shaders/fragment_shader.fs"
#define BUFFER_OFFSET(i) ((char *)NULL + (i))

#include "lib/helper/shape/shape.h"
#include "lib/helper/transformation/transformation.h"
#include "lib/helper/color/color.h"

#include <iostream>
#include <vector>

using namespace std;

void glutInitDisplayModeAbstr(unsigned int mode);
void loadShader(GLuint shaderID, const char *shaderFileHandle);
char *readShader(const char *fileName);
void checkForShaderError(GLuint shaderID);
void checkForProgramError(void);
void installShaders(void);
void display(void);
void enableAlign(GLuint buffer, GLuint id, GLint count, GLenum type, GLboolean normalized, GLint stride, GLint offset);
void draw(Shape shape, vector<mat4> translate, vector<mat4> rotate, vector<mat4> scale, vector<Color> color, int count);

#endif //HW4_H