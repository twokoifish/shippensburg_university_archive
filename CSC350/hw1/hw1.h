/**
 * @file hw1.h
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Header file for a simple OpenGL program that displays a 32x32 image of Empoleon.
 * @version 0.1
 * @date 2021-09-08
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef HW_1_HEADER // If there is no header defined for HW1, then define one.
#define HW_1_HEADER

#define STEP_SIZE_FLOAT 0.0625f // The stepsize for each pixel.
#define IMAGE_X 32 // The x dimension of the image.
#define IMAGE_Y 32 // The y dimension of the image.
#define VERT_PER_PIXEL 6 // The number of vertices per pixel.
#define BUFFER_OFFSET(i) ((char *)NULL + (i)) // The buffer offset for the GPU.
#define VERTEX_SHADER "VertexShader.glsl" // The name of the vertex shader.
#define FRAGMENT_SHADER "FragmentShader.glsl" // The name of the fragment shader.
#define WINDOW_X 512 // The x dimension of the window.
#define WINDOW_Y 512 // The y dimension of the window.
#define WINDOW_POS_X 0 // The x position of the window.
#define WINDOW_POS_Y 0 // The y position of the window.
#define WINDOW_TITLE "[CSC 350] Homework #1" // The name of the program window.
#define DEBUG_MODE 0 // Toggle for debug mode.

// Wrap OS specific headers.
#ifdef __APPLE__ // macOS
#define GL_SILENCE_DEPRECATION
#include <OpenGL/gl3.h>   // Core library
#define __gl_h_ 
#include <OpenGL/glu.h>   // Convenience functions
#include <OpenGL/gl3ext.h> // Implemented OpenGL extensions
#include <GLUT/glut.h>    // Display window
#else                     // Linux
#include <GL/glew.h>      // Core OpenGL
#include <GL/glut.h>      // Display Window
#endif

#include <iostream> // Header for cout and endl
#include <vector> // Header for C++ vectors.

using namespace std; // Set namespace to avoid "std::" prefix.

// Struct for Colors. Holds R, G, and B values.
const typedef struct HW1_COLOR_DATA
{
  GLfloat r; // R value for the color between 0 and 1.
  GLfloat g; // G value for the color between 0 and 1.
  GLfloat b; // B value for the color between 0 and 1.
} ColorData;

ColorData y1 = {0.97f, 0.84f, 0.09f}; // lightest yellow
ColorData y2 = {0.87f, 0.65f, 0.03f}; // middle yellow
ColorData y3 = {0.56f, 0.40f, 0.06f}; // darkest yellow
ColorData t1 = {0.53f, 0.50f, 0.34f}; // tan, only used along beak
ColorData b1 = {0.09f, 0.56f, 0.97f}; // lightest blue
ColorData b2 = {0.06f, 0.37f, 0.81f}; // middle blue
ColorData b3 = {0.00f, 0.25f, 0.62f}; // darkest blue
ColorData w1 = {0.94f, 0.94f, 0.94f}; // lightest white
ColorData w2 = {0.81f, 0.81f, 0.81f}; // middle white
ColorData w3 = {0.59f, 0.62f, 0.62f}; // darkest white
ColorData k1 = {0.28f, 0.31f, 0.34f}; // lightest black, almost grey
ColorData k2 = {0.21f, 0.25f, 0.31f}; // purpleish black
ColorData k3 = {0.18f, 0.21f, 0.25f}; // darker purple black
ColorData k4 = {0.15f, 0.18f, 0.21f}; // darker black
ColorData bg = {1.00f, 0.89f, 0.54f}; // orange background

// Map of colors that make the Empoleon image.
const ColorData colorData[] = {
  bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,k4,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,k4,bg,k4,y1,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,k4,y2,k4,y1,y1,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,bg,bg,k4,y2,k4,y2,y1,y1,k4,k4,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,bg,bg,k4,y2,b2,y1,y2,y1,k2,k2,k2,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,bg,k4,y2,b2,b2,y1,y2,k2,k1,k2,k2,y1,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,bg,k4,y2,b2,y1,y1,k2,k1,k1,k1,y2,y1,k2,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,bg,k4,y2,b2,y1,y2,k1,k1,k1,y2,y1,y1,k2,k2,k4,k4,k4,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,bg,k4,y2,b2,y1,b1,k1,b1,k1,k1,y1,y1,k2,k2,k4,b2,k4,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,k4,b2,y2,y1,y1,b1,b1,b1,b1,k1,y1,y2,k2,k4,b2,b2,k4,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,k4,b2,y2,y1,y2,b2,k1,k1,w2,k1,y1,y2,k4,b2,b2,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,k4,b1,y2,y1,y2,b3,w1,b3,w1,y3,y2,y3,b2,b2,b3,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,k4,b1,y2,y1,y2,k1,b2,b2,y2,y2,t1,b2,b2,b3,k4,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,k4,k4,b1,y2,y1,y2,y2,y2,y2,y2,t1,b2,b2,b3,k4,k3,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,k4,k4,k1,k1,b1,b1,y1,y1,y1,y2,t1,b2,b2,k2,k3,k3,k3,k3,k4,k4,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,k4,b1,b1,k1,k4,b2,b1,b1,b2,b2,b2,b2,k2,k2,k1,k1,k1,k1,k3,k4,k3,k4,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,k4,k1,b1,b2,k3,k4,w2,b1,b2,b2,b2,w3,k2,k1,k1,k1,k1,k1,k1,k3,k4,b2,b3,k4,k4,bg,bg,bg,bg,bg,
  bg,k4,k1,b1,k3,k3,k4,w2,w2,b1,b2,b2,w2,w2,k1,k1,k1,k1,k1,k1,k1,k2,k3,k4,b2,b2,b2,k4,bg,bg,bg,bg,
  bg,k4,b1,k2,k2,y2,k4,k1,w1,b1,b2,w1,w1,w1,w2,k1,k1,k1,k1,k1,k1,k2,k2,k1,k3,k4,k4,k4,bg,bg,bg,bg,
  bg,k4,b2,k3,y2,k3,k4,w2,b1,b1,w2,w1,w1,k1,k1,k1,k1,k1,k1,k2,k3,k2,k2,k1,k1,k3,b3,b3,k4,bg,bg,bg,
  bg,k4,k4,k4,k4,bg,k4,w2,b1,b1,w2,w1,w1,w3,k1,k1,k1,k1,k1,k2,k3,k2,k2,k2,k1,k1,k2,k3,k4,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,k4,w3,b1,b1,w2,w1,w1,w2,k1,k1,k1,k1,k1,k2,k4,b1,b2,b2,k1,k1,k1,k2,k3,k4,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,k4,b1,b2,w2,w1,w1,w1,w3,k1,k1,k1,k1,k2,k4,b2,b1,b1,k1,k1,k1,k1,k3,k3,k4,bg,
  bg,bg,bg,bg,bg,bg,k4,b3,b2,b2,w2,w1,w1,w1,w1,w3,k1,k1,k2,k2,k3,k4,b1,b1,b2,k1,k1,k1,k1,b3,k4,bg,
  bg,bg,bg,bg,bg,bg,k4,b2,b2,b2,k1,w1,k1,w1,k1,w2,w3,k2,k3,k3,k3,k3,k4,k4,b1,b1,b2,k1,k1,k3,b2,k4,
  bg,bg,bg,bg,bg,bg,bg,k4,b3,b2,k1,w2,k1,w2,k1,w3,k2,k2,k3,k3,k3,k3,k4,bg,k4,k4,b1,b2,k1,k3,b3,k4,
  bg,bg,bg,bg,bg,bg,bg,bg,k4,b3,b3,w3,w3,w3,w3,k2,k3,k3,k3,k3,k4,k4,bg,bg,bg,bg,k4,k4,b2,b2,b2,k4,
  bg,bg,bg,bg,bg,bg,bg,bg,k4,y2,y2,y3,k4,k4,k4,k4,k4,k4,k4,k4,y3,k4,bg,bg,bg,bg,bg,bg,k4,k4,k4,k4,
  bg,bg,bg,bg,bg,bg,bg,bg,bg,k4,k4,k4,bg,bg,bg,k4,y2,y2,y1,y2,y2,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,k4,y1,y2,y1,y2,y3,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,k4,k4,k4,y3,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,
  bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,k4,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg,bg
};

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
char* readShaderCode(const char* filename);

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
 * @brief Use the data provided to OpenGL to draw the image.
 * 
 */
void paintGL(void);

#endif