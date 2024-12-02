/**
 * @file hw2.h
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Header file for a simple OpenGL program that displays a 32x32 image of Empoleon.
 * @version 0.1
 * @date 2021-09-29
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef HW_2_HEADER // If there is no header defined for HW2, then define one.
#define HW_2_HEADER

// Wrap OS specific headers.
#ifdef __APPLE__            // macOS
#define GL_SILENCE_DEPRECATION
#include <OpenGL/gl3.h>     // Core library
#define __gl_h_
#include <OpenGL/glu.h>     // Convenience functions
#include <OpenGL/gl3ext.h>  // Implemented OpenGL extensions
#include <GLUT/glut.h>      // Display window
#else                       // Linux
#include <GL/glew.h>        // Core OpenGL
#include <GL/glut.h>        // Display Window
#endif

#include <iostream>         // Header for cout and endl
#include <vector>           // Header for C++ vectors.
#include "matrix/matrix4.h" // Header for custom matrix class.

#define STEP_SIZE_FLOAT 1.0f               // The stepsize for each pixel.
#define IMAGE_X 32                            // The x dimension of the image.
#define IMAGE_Y 32                            // The y dimension of the image.
#define VERT_PER_FACE 6                       // The number of vertices per face.
#define NUM_FACES_CUBE 6                      // The number of faces on a cube.
#define BUFFER_OFFSET(i) ((char *)NULL + (i)) // The buffer offset for the GPU.
#define VERTEX_SHADER "VertexShader.glsl"     // The name of the vertex shader.
#define FRAGMENT_SHADER "FragmentShader.glsl" // The name of the fragment shader.
#define WINDOW_X 512.0                          // The x dimension of the window.
#define WINDOW_Y 512.0                          // The y dimension of the window.
#define WINDOW_POS_X 0                        // The x position of the window.
#define WINDOW_POS_Y 0                        // The y position of the window.
#define WINDOW_TITLE "[CSC 350] Homework #2"  // The name of the program window.
#define DEBUG_MODE 0                          // Toggle for debug mode.
#define SCALE_UNIFORM 0.25                       // The scale for the image.
#define ROTATION_X 315                        // The X rotation for the image.
#define ROTATION_Y 0                          // The Y rotation for the image.
#define ROTATION_Z 315                        // The Z rotation for the image.
#define TRANSLATE_X 0                         // The X translation for the image.
#define TRANSLATE_Y 0                         // The Y translation for the image.
#define TRANSLATE_Z 10.0                      // The Z translation for the image.
#define PERSPECTIVE_FAR 100.0                  // The far value for the perspective.
#define PERSPECTIVE_NEAR 5.0                  // The near value for the perspective
#define PERSPECTIVE_DEGREES 90.0              // The E value for the perspective matrix.

using namespace std; // Set namespace to avoid "std::" prefix.

// Struct for Colors. Holds R, G, and B values.
const typedef struct HW2_COLOR_DATA
{
  GLfloat r; // R value for the color between 0 and 1.
  GLfloat g; // G value for the color between 0 and 1.
  GLfloat b; // B value for the color between 0 and 1.
  GLfloat a; // A value for the color between 0 and 1.
} ColorData;

ColorData d1 = {0.97f, 0.84f, 0.09f, 1.00f}; // lightest yellow, changed to d# to be compatable with cmath.
ColorData d2 = {0.87f, 0.65f, 0.03f, 1.00f}; // middle yellow, changed to d# to be compatable with cmath.
ColorData d3 = {0.56f, 0.40f, 0.06f, 1.00f}; // darkest yellow, changed to d# to be compatable with cmath.
ColorData t1 = {0.53f, 0.50f, 0.34f, 1.00f}; // tan, only used along beak
ColorData b1 = {0.09f, 0.56f, 0.97f, 1.00f}; // lightest blue
ColorData b2 = {0.06f, 0.37f, 0.81f, 1.00f}; // middle blue
ColorData b3 = {0.00f, 0.25f, 0.62f, 1.00f}; // darkest blue
ColorData w1 = {0.94f, 0.94f, 0.94f, 1.00f}; // lightest white
ColorData w2 = {0.81f, 0.81f, 0.81f, 1.00f}; // middle white
ColorData w3 = {0.59f, 0.62f, 0.62f, 1.00f}; // darkest white
ColorData k1 = {0.28f, 0.31f, 0.34f, 1.00f}; // lightest black, almost grey
ColorData k2 = {0.21f, 0.25f, 0.31f, 1.00f}; // purpleish black
ColorData k3 = {0.18f, 0.21f, 0.25f, 1.00f}; // darker purple black
ColorData k4 = {0.15f, 0.18f, 0.21f, 1.00f}; // darker black
ColorData bg = {1.00f, 0.89f, 0.54f, 1.00f}; // orange background
ColorData sd = {0.95f, 0.47f, 0.29f, 1.00f}; // side color, darker orange.
ColorData tp = {0.85f, 0.50f, 0.30f, 1.00f}; // top color, sorta orange.
ColorData bt = {0.95f, 0.50f, 0.30f, 1.00f}; // bottom color, sorta orange.

// Map of colors that make the Empoleon image.
const ColorData colorData[] = {
    bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, k4, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, k4, bg, k4, d1, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, k4, d2, k4, d1, d1, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, bg, bg, k4, d2, k4, d2, d1, d1, k4, k4, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, bg, bg, k4, d2, b2, d1, d2, d1, k2, k2, k2, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, bg, k4, d2, b2, b2, d1, d2, k2, k1, k2, k2, d1, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, bg, k4, d2, b2, d1, d1, k2, k1, k1, k1, d2, d1, k2, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, bg, k4, d2, b2, d1, d2, k1, k1, k1, d2, d1, d1, k2, k2, k4, k4, k4, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, bg, k4, d2, b2, d1, b1, k1, b1, k1, k1, d1, d1, k2, k2, k4, b2, k4, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, k4, b2, d2, d1, d1, b1, b1, b1, b1, k1, d1, d2, k2, k4, b2, b2, k4, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, k4, b2, d2, d1, d2, b2, k1, k1, w2, k1, d1, d2, k4, b2, b2, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, k4, b1, d2, d1, d2, b3, w1, b3, w1, d3, d2, d3, b2, b2, b3, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, k4, b1, d2, d1, d2, k1, b2, b2, d2, d2, t1, b2, b2, b3, k4, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, k4, k4, b1, d2, d1, d2, d2, d2, d2, d2, t1, b2, b2, b3, k4, k3, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, k4, k4, k1, k1, b1, b1, d1, d1, d1, d2, t1, b2, b2, k2, k3, k3, k3, k3, k4, k4, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, k4, b1, b1, k1, k4, b2, b1, b1, b2, b2, b2, b2, k2, k2, k1, k1, k1, k1, k3, k4, k3, k4, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, k4, k1, b1, b2, k3, k4, w2, b1, b2, b2, b2, w3, k2, k1, k1, k1, k1, k1, k1, k3, k4, b2, b3, k4, k4, bg, bg, bg, bg, bg,
    bg, k4, k1, b1, k3, k3, k4, w2, w2, b1, b2, b2, w2, w2, k1, k1, k1, k1, k1, k1, k1, k2, k3, k4, b2, b2, b2, k4, bg, bg, bg, bg,
    bg, k4, b1, k2, k2, d2, k4, k1, w1, b1, b2, w1, w1, w1, w2, k1, k1, k1, k1, k1, k1, k2, k2, k1, k3, k4, k4, k4, bg, bg, bg, bg,
    bg, k4, b2, k3, d2, k3, k4, w2, b1, b1, w2, w1, w1, k1, k1, k1, k1, k1, k1, k2, k3, k2, k2, k1, k1, k3, b3, b3, k4, bg, bg, bg,
    bg, k4, k4, k4, k4, bg, k4, w2, b1, b1, w2, w1, w1, w3, k1, k1, k1, k1, k1, k2, k3, k2, k2, k2, k1, k1, k2, k3, k4, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, k4, w3, b1, b1, w2, w1, w1, w2, k1, k1, k1, k1, k1, k2, k4, b1, b2, b2, k1, k1, k1, k2, k3, k4, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, k4, b1, b2, w2, w1, w1, w1, w3, k1, k1, k1, k1, k2, k4, b2, b1, b1, k1, k1, k1, k1, k3, k3, k4, bg,
    bg, bg, bg, bg, bg, bg, k4, b3, b2, b2, w2, w1, w1, w1, w1, w3, k1, k1, k2, k2, k3, k4, b1, b1, b2, k1, k1, k1, k1, b3, k4, bg,
    bg, bg, bg, bg, bg, bg, k4, b2, b2, b2, k1, w1, k1, w1, k1, w2, w3, k2, k3, k3, k3, k3, k4, k4, b1, b1, b2, k1, k1, k3, b2, k4,
    bg, bg, bg, bg, bg, bg, bg, k4, b3, b2, k1, w2, k1, w2, k1, w3, k2, k2, k3, k3, k3, k3, k4, bg, k4, k4, b1, b2, k1, k3, b3, k4,
    bg, bg, bg, bg, bg, bg, bg, bg, k4, b3, b3, w3, w3, w3, w3, k2, k3, k3, k3, k3, k4, k4, bg, bg, bg, bg, k4, k4, b2, b2, b2, k4,
    bg, bg, bg, bg, bg, bg, bg, bg, k4, d2, d2, d3, k4, k4, k4, k4, k4, k4, k4, k4, d3, k4, bg, bg, bg, bg, bg, bg, k4, k4, k4, k4,
    bg, bg, bg, bg, bg, bg, bg, bg, bg, k4, k4, k4, bg, bg, bg, k4, d2, d2, d1, d2, d2, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, k4, d1, d2, d1, d2, d3, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, k4, k4, k4, d3, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg,
    bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, k4, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg, bg};

/**
 * @brief Initialize glut with the command line arguments.
 * 
 * @param argc the number of arguments passed with the program call.
 * @param argv the arguments passed with the program call.
 */
void initializeGLUT(int argc, char **argv);

/**
 * @brief Abstraction of glutInitDisplay(unsigned int) that supports both macOS and Linux.
 * 
 * @param mode the modes to initialize OpenGL under.
 */
void glutInitDisplayModeAbstraction(unsigned int mode);

/**
 * @brief Prints information about OpenGL, the renderer, and GLSL.
 * 
 */
void printSessionInformation(void);

/**
 * @brief Initializes OpenGL.
 * 
 */
void initializeGL(void);

/**
 * @brief Abstraction of glewInit(void) that supports both macOS and Linux.
 * 
 */
void glewInitAbstraction(void);

/**
 * @brief Initialize the vertex array object for the vertex buffer objects.
 * 
 */
void initializeVAO(void);

/**
 * @brief Build a vector containing vertices and color data for triangles.
 * 
 * @return vector<GLfloat> a vector containing vertices and color data for triangles.
 */
vector<GLfloat> buildVerticesWithColor(void);

/**
 * @brief Allows the user to install vertex and fragment shaders.
 * 
 */
void installShaders(void);

/**
 * @brief Reads in a GLSL file and returns it for processing.
 * 
 * @param filename the file name.
 * @return char* the file data.
 */
char *readShaderCode(const char *filename);

/**
 * @brief Check the given shader for any compilation errors.
 * 
 * @param shader the id of the shader.
 */
void checkForShaderError(GLuint shader);

/**
 * @brief Check the program for any compilation errors.
 * 
 * @param programID the id of the program.
 */
void checkForProgramError(GLuint programID);

/**
 * @brief Creates a matrix with transformations from predefined parameters.
 * 
 * @return Matrix4 with transformations from predefined parameters.
 */
Matrix4 buildDefinedTransformation(void);

/**
 * @brief Use the data provided to OpenGL to draw the image.
 * 
 */
void paintGL(void);

#endif
