#include "final.h"

GLuint programID;
GLuint vao;
float rotationKey = 0;
float heightKey = 0;
float heightKeyAdj = 0.05;

/**
 * @brief main loop, starts the project.
 * 
 * @param argc command arg count.
 * @param argv command arg 2d string.
 * @return int exit code.
 */
int main(int argc, char **argv)
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_3_2_CORE_PROFILE | GLUT_DEPTH | GLUT_RGBA);
  glutInitWindowSize(1024, 1024);
  glutInitWindowPosition(0, 0);
  glutCreateWindow("Final Project");
  glEnable(GL_DEPTH_TEST);
  installShaders();
  glutDisplayFunc(display);
  glutMainLoop();
  return 0;
}

/**
 * @brief loads shader and checks it for error.
 * 
 * @param shaderID the id for the shader.
 * @param shaderFileHandle the file name of the shader.
 */
void loadShader(const GLuint shaderID, const char *shaderFileHandle)
{
  cout << "Loading " << shaderFileHandle << endl;
  const char *adapter[1];
  adapter[0] = readShader(shaderFileHandle);
  glShaderSource(shaderID, 1, adapter, 0);
  glCompileShader(shaderID);
  checkForShaderError(shaderID);
  glAttachShader(programID, shaderID);
}

/**
 * @brief reads shader into memory.
 * 
 * @param fileName name of shader.
 * @return char* pointer to shader mem.
 */
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

/**
 * @brief checks for shader errors.
 * 
 * @param shaderID shader id.
 */
void checkForShaderError(const GLuint shaderID)
{
  GLint compileStatus;
  glGetShaderiv(shaderID, GL_COMPILE_STATUS, &compileStatus);
  if (compileStatus != GL_TRUE)
  {
    cout << "Error " << shaderID << endl;
    GLint maxLength;
    glGetShaderiv(shaderID, GL_INFO_LOG_LENGTH, &maxLength);
    GLcharARB *errorLog = new GLchar[maxLength];
    GLsizei bufferSize;
    glGetShaderInfoLog(shaderID, maxLength, &bufferSize, errorLog);
    cout << errorLog << endl;
    delete[] errorLog;
    exit(EXIT_FAILURE);
  }
}

/**
 * @brief checks for program errors.
 * 
 */
void checkForProgramError(void)
{
  GLint linkStatus;
  glGetProgramiv(programID, GL_LINK_STATUS, &linkStatus);
  if (linkStatus != GL_TRUE)
  {
    GLint maxLength;
    glGetProgramiv(programID, GL_INFO_LOG_LENGTH, &maxLength);
    GLcharARB *errorLog = new GLchar[maxLength];
    GLsizei bufferSize;
    glGetProgramInfoLog(programID, maxLength, &bufferSize, errorLog);
    cout << errorLog << endl;
    delete[] errorLog;
    exit(EXIT_FAILURE);
  }
}

/**
 * @brief installs shaders
 * 
 */
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

/**
 * @brief displays shapes to screen.
 * 
 */
void display(void)
{
  // Clear the display buffer.
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  
  // Colors used in the scene.
  // Red 
  Color r = Color(1.0f, 0.0f, 0.0f, 1.00f);
  // Orange
  Color o = Color(1.0f, 0.6f, 0.2f, 1.00f);
  // Yellow
  Color y = Color(1.0f, 1.0f, 0.4f, 1.00f);
  // Green
  Color g = Color(0.0f, 1.0f, 0.0f, 1.00f);
  // Blue
  Color b = Color(1.0f, 0.0f, 1.0f, 1.00f);
  // Grey
  Color k = Color(0.8f, 0.8f, 1.8f, 1.00f);

  // Shapes used for constuction
  // Cube
  Shape cube = Shape(ST_CUBE);
  // Sphere
  Shape sphere = Shape(ST_SPHERE);
  // Octaheadron
  Shape octaheadron = Shape(ST_OCTAHEDRON);
  // Cylinder
  Shape cylinder = Shape(ST_CYLINDER);

  // Used as the parent of everything
  Component floor = Component(programID, cube, g, "Floor, Base");
  floor.setTranslate(0.0, -10.0, 50.0);
  floor.setScale(5000.0, 0.0, 5000.0);

  // Link add path
  Component path = Component(programID, cube, o, "Floor, Path");
  path.setTranslate(0.0, 0.1, 0.0);
  path.setScale(15.0/5000.0, 0.0, 5000.0);

  // Link path to floor.
  floor.addChild(&floor, &path);

  // Build trunk
  Component tree_trunk1 = Component(programID, cylinder, o, "Tree, Trunk");
  tree_trunk1.setTranslate(-20 * (1.0 / 5000.0), 1.0, 0.0);
  tree_trunk1.setScale(5.0/5000.0, 20.0, 5.0/5000.0);
  tree_trunk1.setRotate(90.0, 0.0, 0.0);

  // Link leaves to trunk
  Component tree_leaves1 = Component(programID, sphere, g, "Tree, Leaves");
  tree_leaves1.setTranslate(0.0, 0.0, -1.0);
  tree_leaves1.setScale(5.0, 5.0, 1.25);

  // Link trunk to floor
  tree_trunk1.addChild(&tree_trunk1, &tree_leaves1);
  floor.addChild(&floor, &tree_trunk1);

  // Chimney for the roof.
  Component house_chimney1 = Component(programID, cube, r, "House, Chimney");
  house_chimney1.setTranslate(0.5, -0.25, 0.0);
  house_chimney1.setScale(0.25, 1.5, 0.25);

  // Roof for the house base.
  Component house_roof1 = Component(programID, octaheadron, k, "House, Roof");
  house_roof1.setTranslate(0.0, 0.5, 0.0);

  // Base of the house
  Component house_base1 = Component(programID, cube, b, "House, Base");
  house_base1.setTranslate(-20.0 * (1.0 / 5000.0), 5.0, -20 * (1.0 / 5000.0));
  house_base1.setScale(10.0 / 5000.0, 10.0, 10.0 / 5000.0);
  house_base1.setRotate(0.0, -90.0, 0.0);

  // Link it all together and add the house as a child.
  house_roof1.addChild(&house_roof1, &house_chimney1);
  house_base1.addChild(&house_base1, &house_roof1);
  floor.addChild(&floor, &house_base1);

  // Chimney for the roof of the second house.
  Component house_chimney2 = Component(programID, cube, o, "House, Chimney");
  house_chimney2.setTranslate(0.5, -0.25, 0.0);
  house_chimney2.setScale(0.25, 1.5, 0.25);

  // Roof for the base of the second house.
  Component house_roof2 = Component(programID, octaheadron, r, "House, Roof");
  house_roof2.setTranslate(0.0, 0.5, 0.0);

  // Base of the second house.
  Component house_base2 = Component(programID, cube, k, "House, Base");
  house_base2.setTranslate(20.0 * (1.0 / 5000.0), 5.0, -5 * (1.0 / 5000.0));
  house_base2.setScale(10.0 / 5000.0, 10.0, 10.0 / 5000.0);
  house_base2.setRotate(0.0, 180.0, 0.0);

  // Link it all together and add the second house as a child.
  house_roof2.addChild(&house_roof2, &house_chimney2);
  house_base2.addChild(&house_base2, &house_roof2);
  floor.addChild(&floor, &house_base2);

  // Chimney for the roof of the second house.
  Component house_chimney3 = Component(programID, cube, y, "House, Chimney");
  house_chimney3.setTranslate(0.5, -0.25, 0.0);
  house_chimney3.setScale(0.25, 1.5, 0.25);

  // Roof for the base of the second house.
  Component house_roof3 = Component(programID, octaheadron, o, "House, Roof");
  house_roof3.setTranslate(0.0, 0.5, 0.0);

  // Base of the second house.
  Component house_base3 = Component(programID, cube, g, "House, Base");
  house_base3.setTranslate(20.0 * (1.0 / 5000.0), 5.0, -20 * (1.0 / 5000.0));
  house_base3.setScale(10.0 / 5000.0, 10.0, 10.0 / 5000.0);
  house_base3.setRotate(0.0, 90.0, 0.0);

  // Link it all together and add the second house as a child.
  house_roof3.addChild(&house_roof3, &house_chimney3);
  house_base3.addChild(&house_base3, &house_roof3);
  floor.addChild(&floor, &house_base3);

  // Core of the body, all parts connect to it.
  Component body_torso = Component(programID, cube, b, "Body, Torso");
  body_torso.setTranslate(0.0 * (1.0 / 5000.0), 7.5 + heightKey, -25 * (rotationKey / 180.0) * (1.0 / 5000.0));
  body_torso.setScale(2.0 / 5000.0, 3.0, 0.7 / 5000.0);
  body_torso.setRotate(0.0, rotationKey, 0.0);

  // Joint for the right arm
  Component body_right_arm_joint = Component(programID, cube, o, "Body, Right Arm, Joint");
  body_right_arm_joint.setTranslate(0.5, 0.44, 0.0);
  body_right_arm_joint.setScale(0.125, 0.083, 0.125);
  body_right_arm_joint.setRotate(rotationKey, 0.0, 0.0);

  // Right arm
  Component body_right_arm = Component(programID, cube, g, "Body, Right Arm");
  body_right_arm.setTranslate(1.9, -5.0, 0.0);
  body_right_arm.setScale(0.5/0.125, 1.0/0.083, 0.5/0.125);
  
  // Joint for the left arm.
  Component body_left_arm_joint = Component(programID, cube, o, "Body, Left Arm, Joint");
  body_left_arm_joint.setTranslate(-0.5, 0.44, 0.0);
  body_left_arm_joint.setScale(0.125, 0.083, 0.125);
  body_left_arm_joint.setRotate(-rotationKey, 0.0, 0.0);

  // Left arm.
  Component body_left_arm = Component(programID, cube, r, "Body, Left Arm");
  body_left_arm.setTranslate(-1.9, -5.0, 0.0);
  body_left_arm.setScale(0.5/0.125, 1.0/0.083, 0.5/0.125);

  // Head.
  Component body_head = Component(programID, cube, b, "Body, Head");
  body_head.setTranslate(0.0, 0.65, 0.0);
  body_head.setScale(0.5, 0.325, 0.5);

  // Base for the hat that sits on the head.
  Component body_head_hat_base = Component(programID, cylinder, y, "Body, Head, Hat Base");
  body_head_hat_base.setTranslate(0.0, 0.55, 0.0);
  body_head_hat_base.setScale(1.5, 0.125, 1.5);
  body_head_hat_base.setRotate(90.0, 0.0, -rotationKey);

  // Top for the hat that sits on the head.
  Component body_head_hat_top = Component(programID, cylinder, y, "Body, Head, Hat Top");
  body_head_hat_top.setTranslate(0.0, 0.0, -3.0);
  body_head_hat_top.setScale(0.5, 0.5, 5.0);

  // Joint for the right leg.
  Component body_right_leg_joint = Component(programID, cube, o, "Body, Right Leg, Joint");
  body_right_leg_joint.setTranslate(0.30, -0.5, 0.0);
  body_right_leg_joint.setScale(0.125, 0.0625, 0.125);
  body_right_leg_joint.setRotate(heightKey + (heightKeyAdj * 60), 0.0, 0.0);

  // Joint for the left leg.
  Component body_left_leg_joint = Component(programID, cube, o, "Body, Left Leg, Joint");
  body_left_leg_joint.setTranslate(-0.30, -0.5, 0.0);
  body_left_leg_joint.setScale(0.125, 0.0625, 0.125);
  body_left_leg_joint.setRotate(-(heightKey + (heightKeyAdj * 60)), 0.0, 0.0);

  // Right leg.
  Component body_right_leg = Component(programID, cube, r, "Body, Right Leg");
  body_right_leg.setTranslate(-0.3, -7.5, 0.0);
  body_right_leg.setScale(4.0, 15.0, 4.0);

  // Left leg.
  Component body_left_leg = Component(programID, cube, g, "Body, Left Leg");
  body_left_leg.setTranslate(0.3, -7.5, 0.0);
  body_left_leg.setScale(4.0, 15.0, 4.0);

  // Right foot.
  Component body_right_foot = Component(programID, cylinder, g, "Body, Right Foot");
  body_right_foot.setTranslate(0.0, -0.5, -0.25);
  body_right_foot.setScale(1.0, 0.25, 1.5);

  // Left foot.
  Component body_left_foot = Component(programID, cylinder, r, "Body, Left Foot");
  body_left_foot.setTranslate(0.0, -0.5, -0.25);
  body_left_foot.setScale(1.0, 0.25, 1.5);

  body_torso.setScale(0.5, 0.5, 0.5);
  // link foot to leg.
  body_left_leg.addChild(&body_left_leg, &body_left_foot);
  // Link foot to leg.
  body_right_leg.addChild(&body_right_leg, &body_right_foot);
  // link leg to joint.
  body_left_leg_joint.addChild(&body_left_leg_joint, &body_left_leg);
  // link leg to joint.
  body_right_leg_joint.addChild(&body_right_leg_joint, &body_right_leg);
  // link joint to torso.
  body_torso.addChild(&body_torso, &body_left_leg_joint);
  // link joint to torso.
  body_torso.addChild(&body_torso, &body_right_leg_joint);
  // link hat top to base.
  body_head_hat_base.addChild(&body_head_hat_base, &body_head_hat_top);
  // link hat to head.
  body_head.addChild(&body_head, &body_head_hat_base);
  // link head to torso.
  body_torso.addChild(&body_torso, &body_head);
  // link arm to joint.
  body_left_arm_joint.addChild(&body_left_arm_joint, &body_left_arm);
  // link arm to joint.
  body_right_arm_joint.addChild(&body_right_arm_joint, &body_right_arm);
  // link joint to torso. 
  body_torso.addChild(&body_torso, &body_left_arm_joint);
  // link joint to torso.
  body_torso.addChild(&body_torso, &body_right_arm_joint);
  // link torso to floor.
  floor.addChild(&floor, &body_torso);
  // display floor.
  floor.display();

  // Push all components to the GPU.
  glFlush();

  // tweak jumping code to either be up or down.
  if (fabs(heightKey - 2.0) < 0.001)
  {
    heightKeyAdj = -0.05;
  }
  else if (fabs(heightKey - 0.0) < 0.001)
  {
    heightKeyAdj = 0.05;
  }

  // adjust jump height
  heightKey = heightKey + heightKeyAdj;

  // Increment or reset the display key so it does not overflow.
  rotationKey = (rotationKey >= 360)
  ? 0
  : rotationKey += 1;

  // Call display function again.
  glutPostRedisplay();
}
