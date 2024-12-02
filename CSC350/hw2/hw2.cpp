/**
 * @file hw2.cpp
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Simple OpenGL program that displays a 32x32 image of Empoleon.
 * @version 0.1
 * @date 2021-09-29
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "hw2.h"

/**
 * @brief Main method used to start GLUT, GL, and the display.
 * 
 * @param argc the number of command line arguments.
 * @param argv the command line arguments.
 * @return int return 0 on success.
 */
int main(int argc, char **argv)
{
  // Initialize GLUT with command line arguments.
  initializeGLUT(argc, argv);
  // Initialize OpenGL.
  initializeGL();
  // Display the image fed to the GPU.
  glutDisplayFunc(paintGL);
  // Repeat this image.
  glutMainLoop();
  // Return 0 to fulfill int return.
  return 0;
}

/**
 * @brief Initialize glut with the command line arguments.
 * 
 * @param argc the number of arguments passed with the program call.
 * @param argv the arguments passed with the program call.
 */
void initializeGLUT(int argc, char **argv)
{
  // Initialize GLUT.
  glutInit(&argc, argv);
  // Set the display mode.
  glutInitDisplayModeAbstraction(GLUT_DEPTH | GLUT_RGB);
  // Set the window dimensions.
  glutInitWindowSize(WINDOW_X, WINDOW_Y);
  // Set the window position.
  glutInitWindowPosition(WINDOW_POS_X, WINDOW_POS_Y);
  // Set the window title.
  glutCreateWindow(WINDOW_TITLE);
  // Enable DEPTH TEST for OpenGL.
  glEnable(GL_DEPTH_TEST);
  // If debug mode, print session information for OpenGL, the renderer, and GLSL.
  if (DEBUG_MODE)
  {
    printSessionInformation();
  }
}

/**
 * @brief Abstraction of glutInitDisplay(unsigned int) that supports both macOS and Linux.
 * 
 * @param mode the modes to initialize OpenGL under.
 */
void glutInitDisplayModeAbstraction(unsigned int mode)
{
#ifdef __APPLE__
  // If macOS initialize the display mode with the GLUT 3.2 core profile.
  glutInitDisplayMode(mode | GLUT_3_2_CORE_PROFILE);
#else
  // If Linux initialize the display mode with the parameters.
  glutInitDisplayMode(mode);
#endif
}

/**
 * @brief Prints information about OpenGL, the renderer, and GLSL.
 * 
 */
void printSessionInformation(void)
{
  // Print the OpenGL version.
  cout << "OpenGL v" << glGetString(GL_VERSION) << endl;
  // Print the renderer version.
  cout << "Renderer v" << glGetString(GL_RENDERER) << endl;
  // Print the GLSL version.
  cout << "GLSL v" << glGetString(GL_SHADING_LANGUAGE_VERSION) << endl;
}

/**
 * @brief Initializes OpenGL.
 * 
 */
void initializeGL(void)
{
  // Initialize GLEW.
  glewInitAbstraction();
  // Initialize a vertex array object.
  initializeVAO();
  // Build the array of vertices and color.
  vector<GLfloat> vertices = buildVerticesWithColor();
  // The id of the GPU buffer.
  GLuint bufferID;
  // Generate 1 buffer.
  glGenBuffers(1, &bufferID);
  // Bind the buffer to OpenGL.
  glBindBuffer(GL_ARRAY_BUFFER, bufferID);
  // Get the size of the vertices.
  const GLsizeiptr size = vertices.size() * sizeof(GLfloat);
  // Load the vertices and colors into the GPU.
  glBufferData(GL_ARRAY_BUFFER, size, &vertices.front(), GL_STATIC_DRAW);
  // The id for our vertex position.
  const GLuint vPosition = 0;
  // The id for out vertex color.
  const GLuint vColor = 1;
  // The stride from one piece of data to the next.
  const GLsizei stride = sizeof(GLfloat) * 7;
  // Enable vertex position.
  glEnableVertexAttribArray(vPosition);
  // Start the data offset at 0.
  const GLsizei posOffset = sizeof(GLfloat) * 0;
  // Allow OpenGL to walk from vertex to vertex.
  glVertexAttribPointer(vPosition, 3, GL_FLOAT, GL_FALSE, stride, BUFFER_OFFSET(posOffset));
  // Enable vertex color.
  glEnableVertexAttribArray(vColor);
  // Start the data offset at 2.
  const GLsizei colorOffset = sizeof(GLfloat) * 3;
  // Allow OpenGL to walk from color data to color.
  glVertexAttribPointer(vColor, 4, GL_FLOAT, GL_FALSE, stride, BUFFER_OFFSET(colorOffset));
  // Install the OpenGL shaders.
  installShaders();
}

/**
 * @brief Abstraction of glewInit(void) that supports both macOS and Linux.
 * 
 */
void glewInitAbstraction(void)
{
#ifndef __APPLE__
  // If not macOS, initialize GLEW and throw an error if it fails.
  GLenum status = glewInit();
  if (status != GLEW_OK)
  {
    cerr << "Unable to initialize GLEW. Exiting." << endl;
    exit(EXIT_FAILURE);
  }
#else
  // If macOS, GLEW initialization is not required.
  if (DEBUG_MODE)
  {
    cout << "glewInit() is not required on OSX. Skipping." << endl;
  }
#endif
}

/**
 * @brief Initialize the vertex array object for the vertex buffer objects.
 * 
 */
void initializeVAO(void)
{
#ifdef __APPLE__
  // If macOS, create a vertex array object and bind it.
  GLuint vaoID = 0;
  glGenVertexArrays(1, &vaoID);
  glBindVertexArray(vaoID);
#else
  // If Linux, skip VAO initialization.
  if (DEBUG_MODE)
  {
    cout << "VAO not yet needed for Linux. Skipping." << endl;
  }
#endif
}

/**
 * @brief Build a vector containing vertices and color data for triangles.
 * 
 * @return vector<GLfloat> a vector containing vertices and color data for triangles.
 */
vector<GLfloat> buildVerticesWithColor(void)
{
  // Create a vector for the vertices and color.
  vector<GLfloat> vertices;
  // Set the starting index for the color map.
  int vColorIndex = 0;
  // Walk through the y axis of the image in small steps.
  for (GLfloat y = 16.0; y > -16; y -= STEP_SIZE_FLOAT)
  {
    // Walk through the x axis of the image in small steps.
    for (GLfloat x = -16.0; x < 16; x += STEP_SIZE_FLOAT)
    {
      // Get the R, B, and B values for the given "pixel".
      const GLfloat baseR = colorData[vColorIndex].r;
      const GLfloat baseG = colorData[vColorIndex].g;
      const GLfloat baseB = colorData[vColorIndex].b;
      const GLfloat baseA = colorData[vColorIndex].a;

      // Get side colors to attach to the "pixel".
      const GLfloat sideR = sd.r;
      const GLfloat sideG = sd.g;
      const GLfloat sideB = sd.b;
      const GLfloat sideA = sd.a;

      // Get top colors to attach to the "pixel".
      const GLfloat topR = tp.r;
      const GLfloat topG = tp.g;
      const GLfloat topB = tp.b;
      const GLfloat topA = tp.a;

      // Get bottom colors to attach to the "pixel".
      const GLfloat bottomR = bt.r;
      const GLfloat bottomG = bt.g;
      const GLfloat bottomB = bt.b;
      const GLfloat bottomA = bt.a;

      // Get background color to attach to the back of the "pixel".
      const GLfloat backR = bg.r;
      const GLfloat backG = bg.g;
      const GLfloat backB = bg.b;
      const GLfloat backA = bg.a;

      // The predefined front and back Z indexes.
      const GLfloat zback = 0.9;
      const GLfloat zfront = 0.1;

      // Build an array of points and colors that resemble that pixel.
      const GLfloat pixel3d[] = {
          // Front
          x,
          y,
          zfront,
          baseR,
          baseG,
          baseB,
          baseA,
          x + STEP_SIZE_FLOAT,
          y,
          zfront,
          baseR,
          baseG,
          baseB,
          baseA,
          x,
          y - STEP_SIZE_FLOAT,
          zfront,
          baseR,
          baseG,
          baseB,
          baseA,
          x + STEP_SIZE_FLOAT,
          y,
          zfront,
          baseR,
          baseG,
          baseB,
          baseA,
          x,
          y - STEP_SIZE_FLOAT,
          zfront,
          baseR,
          baseG,
          baseB,
          baseA,
          x + STEP_SIZE_FLOAT,
          y - STEP_SIZE_FLOAT,
          zfront,
          baseR,
          baseG,
          baseB,
          baseA,
          // Left
          x,
          y,
          zfront,
          sideR,
          sideG,
          sideB,
          sideA,
          x,
          y - STEP_SIZE_FLOAT,
          zfront,
          sideR,
          sideG,
          sideB,
          sideA,
          x,
          y,
          zback,
          sideR,
          sideG,
          sideB,
          sideA,
          x,
          y,
          zback,
          sideR,
          sideG,
          sideB,
          sideA,
          x,
          y - STEP_SIZE_FLOAT,
          zback,
          sideR,
          sideG,
          sideB,
          sideA,
          x,
          y - STEP_SIZE_FLOAT,
          zfront,
          sideR,
          sideG,
          sideB,
          sideA,
          // Right
          x + STEP_SIZE_FLOAT,
          y,
          zfront,
          sideR,
          sideG,
          sideB,
          sideA,
          x + STEP_SIZE_FLOAT,
          y - STEP_SIZE_FLOAT,
          zfront,
          sideR,
          sideG,
          sideB,
          sideA,
          x + STEP_SIZE_FLOAT,
          y,
          zback,
          sideR,
          sideG,
          sideB,
          sideA,
          x + STEP_SIZE_FLOAT,
          y,
          zback,
          sideR,
          sideG,
          sideB,
          sideA,
          x + STEP_SIZE_FLOAT,
          y - STEP_SIZE_FLOAT,
          zback,
          sideR,
          sideG,
          sideB,
          sideA,
          x + STEP_SIZE_FLOAT,
          y - STEP_SIZE_FLOAT,
          zfront,
          sideR,
          sideG,
          sideB,
          sideA,
          // Top
          x,
          y,
          zfront,
          topR,
          topG,
          topB,
          topA,
          x + STEP_SIZE_FLOAT,
          y,
          zfront,
          topR,
          topG,
          topB,
          topA,
          x,
          y,
          zback,
          topR,
          topG,
          topB,
          topA,
          x,
          y,
          zback,
          topR,
          topG,
          topB,
          topA,
          x + STEP_SIZE_FLOAT,
          y,
          zback,
          topR,
          topG,
          topB,
          topA,
          x + STEP_SIZE_FLOAT,
          y,
          zfront,
          topR,
          topG,
          topB,
          topA,
          // Bottom
          x,
          y - STEP_SIZE_FLOAT,
          zfront,
          bottomR,
          bottomG,
          bottomB,
          bottomA,
          x + STEP_SIZE_FLOAT,
          y - STEP_SIZE_FLOAT,
          zfront,
          bottomR,
          bottomG,
          bottomB,
          bottomA,
          x,
          y - STEP_SIZE_FLOAT,
          zback,
          bottomR,
          bottomG,
          bottomB,
          bottomA,
          x,
          y - STEP_SIZE_FLOAT,
          zback,
          bottomR,
          bottomG,
          bottomB,
          bottomA,
          x + STEP_SIZE_FLOAT,
          y - STEP_SIZE_FLOAT,
          zback,
          bottomR,
          bottomG,
          bottomB,
          bottomA,
          x + STEP_SIZE_FLOAT,
          y - STEP_SIZE_FLOAT,
          zfront,
          bottomR,
          bottomG,
          bottomB,
          bottomA,
          // Back
          x,
          y,
          zback,
          backR,
          backG,
          backB,
          backA,
          x + STEP_SIZE_FLOAT,
          y,
          zback,
          backR,
          backG,
          backB,
          backA,
          x,
          y - STEP_SIZE_FLOAT,
          zback,
          backR,
          backG,
          backB,
          backA,
          x + STEP_SIZE_FLOAT,
          y,
          zback,
          backR,
          backG,
          backB,
          backA,
          x,
          y - STEP_SIZE_FLOAT,
          zback,
          backR,
          backG,
          backB,
          backA,
          x + STEP_SIZE_FLOAT,
          y - STEP_SIZE_FLOAT,
          zback,
          backR,
          backG,
          backB,
          backA,
      };
      // Get the size of the pixel.
      int pixelMemSize = sizeof(pixel3d) / sizeof(pixel3d[0]);
      // Load the pixel into the vector to be passed to the GPU.
      for (int pixel_data = 0; pixel_data < pixelMemSize; pixel_data++)
      {
        vertices.push_back(pixel3d[pixel_data]);
      }
      // Get the next color index.
      vColorIndex++;
    }
  }
  // Return the vector so it can be used in the GPU.
  return vertices;
}

/**
 * @brief Allows the user to install vertex and fragment shaders.
 * 
 */
void installShaders(void)
{
  // Create an id for the vertex shader.
  GLuint vertexShaderID = glCreateShader(GL_VERTEX_SHADER);
  // Create an id for the fragment shader.
  GLuint fragmentShaderID = glCreateShader(GL_FRAGMENT_SHADER);
  // Create a temp char pointer for the shader data.
  const char *adapter[1];
  // Load the shader data into the adapter.
  adapter[0] = readShaderCode(VERTEX_SHADER);
  // Bind the shader data to its respective id.
  glShaderSource(vertexShaderID, 1, adapter, 0);
  // Load the shader data into the adapter.
  adapter[0] = readShaderCode(FRAGMENT_SHADER);
  // Bind the shader data to its respective id.
  glShaderSource(fragmentShaderID, 1, adapter, 0);
  // Compile the vertex shader.
  glCompileShader(vertexShaderID);
  // Check the vertex shader for errors.
  checkForShaderError(vertexShaderID);
  // Compile the fragment shader.
  glCompileShader(fragmentShaderID);
  // Check the fragment shader for errors.
  checkForShaderError(fragmentShaderID);
  // Create an id for the program.
  GLuint programID = glCreateProgram();
  // Link the vertex shader to the program.
  glAttachShader(programID, vertexShaderID);
  // Link the fragment shader to the program.
  glAttachShader(programID, fragmentShaderID);
  // Link the program to OpenGL.
  glLinkProgram(programID);
  // Check the program for errors.
  checkForProgramError(programID);
  // Use the program with OpenGL.
  glUseProgram(programID);
  // Build the transformations for the vertex array.
  Matrix4 transformation = buildDefinedTransformation();
  // Find the address of matrix in the GLSL and pass the transformations to it.
  GLuint matrixID = glGetUniformLocation(programID, "transformedMatrix");
  glUniformMatrix4fv(matrixID, 1, GL_TRUE, &transformation[0][0]);
}

/**
 * @brief Reads in a GLSL file and returns it for processing.
 * 
 * @param filename the file name.
 * @return char* the file data.
 */
char *readShaderCode(const char *filename)
{
  // Open the file.
  FILE *fp = fopen(filename, "r");
  // If null pointer, throw an error.
  if (fp == NULL)
  {
    cout << "File failed to load..." << filename;
    exit(1);
  }
  // Read the file.
  fseek(fp, 0L, SEEK_END);
  int res = ftell(fp);
  fseek(fp, 0, SEEK_SET);
  // Load the data into a char pointer.
  char *data = (char *)calloc(res + 1, sizeof(char));
  fread(data, sizeof(char), res, fp);
  // If debug mode, print the file contents.
  if (DEBUG_MODE)
  {
    cout << data << endl;
  }
  fclose(fp);
  // Return the file data.
  return data;
}

/**
 * @brief Check the given shader for any compilation errors.
 * 
 * @param shader the id of the shader.
 */
void checkForShaderError(GLuint shader)
{
  // Create a compile status.
  GLint compileStatus;
  // Link it to the shader and set its status.
  glGetShaderiv(shader, GL_COMPILE_STATUS, &compileStatus);
  if (compileStatus != GL_TRUE)
  {
    // If an error occurs, print the status and the errors in the file.
    cout << compileStatus << endl;
    GLint maxLength;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &maxLength);
    cout << maxLength << endl;
    GLchar *errorLog = new GLchar[maxLength];
    GLsizei bufferSize;
    glGetShaderInfoLog(shader, maxLength, &bufferSize, errorLog);
    cout << errorLog << endl;
    delete[] errorLog;
  }
}

/**
 * @brief Check the program for any compilation errors.
 * 
 * @param programID the id of the program.
 */
void checkForProgramError(GLuint programID)
{
  // Create a link status.
  GLint linkStatus;
  // Link it to the program and set its status.
  glGetProgramiv(programID, GL_LINK_STATUS, &linkStatus);
  if (linkStatus != GL_TRUE)
  {
    // If an error occurs, print the status and the errors in the file.
    GLint maxLength;
    glGetProgramiv(programID, GL_INFO_LOG_LENGTH, &maxLength);
    GLchar *errorLog = new GLchar[maxLength];
    GLsizei bufferSize;
    glGetProgramInfoLog(programID, maxLength, &bufferSize, errorLog);
    cout << errorLog << endl;
    delete[] errorLog;
  }
}

/**
 * @brief Creates a matrix with transformations from predefined parameters.
 *        ! This function is HARD CODED. This needs to be fixed in future releases.
 * 
 * @return Matrix4 with transformations from predefined parameters.
 */
Matrix4 buildDefinedTransformation(void)
{
  // Build a translation of the X, Y, and Z axis.
  Matrix4 translate = Matrix4::translate(TRANSLATE_X, TRANSLATE_Y, TRANSLATE_Z);
  // Build a uniform scaling of all axis.
  Matrix4 scale = Matrix4::scaleUniform(SCALE_UNIFORM);
  // Build a rotation matrix around the X, Y, and Z axis.
  Matrix4 rotation = Matrix4::rotate(ROTATION_X, ROTATION_Y, ROTATION_Z);
  // Build the perspective matrix.
  Matrix4 perspectiveProjection = Matrix4::perspectiveProjection(PERSPECTIVE_FAR, PERSPECTIVE_NEAR, PERSPECTIVE_DEGREES);
  // Use the above matrices to build the transformation matrix.
  Matrix4 transformation = perspectiveProjection * translate * scale * rotation;
  // Print matrix if in debug mode.
  if (MATRIX_DEBUG)
  {
    cout << "transformation" << endl;
    transformation.print();
  }
  // Return transformation matrix.
  return transformation;
}

/**
 * @brief Use the data provided to OpenGL to draw the image.
 * 
 */
void paintGL(void)
{
  // Clear the buffer.
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  // Draw the GPU buffer.
  GLsizei itemsToDraw = IMAGE_X * IMAGE_Y * VERT_PER_FACE * NUM_FACES_CUBE;
  glDrawArrays(GL_TRIANGLES, 0, itemsToDraw);
  // Flush the GPU buffer.
  glFlush();
}