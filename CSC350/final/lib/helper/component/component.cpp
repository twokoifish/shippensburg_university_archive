/**
 * @file component.cpp
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Implementation of Component class.
 * @version 0.1
 * @date 2021-12-10
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "component.h"

/**
 * @brief Construct a new Component object
 * 
 * @param programID ID of the OpenGL program.
 * @param data Shae data to draw.
 * @param color Color data to apply.
 * @param nickname Component name. Used for debugging.
 */
Component::Component(GLuint programID, Shape shape, Color color, string name)
{
  this->programID = programID;
  this->shape = shape;
  this->color = color;
  this->name = name;
  this->hasParent = false;
  this->parent = NULL;
  this->rotate = Transformation::identity(); 
  this->translate = Transformation::identity();
  this->scale = Transformation::identity();
}

/**
 * @brief Links a child to a parent.
 * 
 * @param parent Pointer to the parent Component.
 * @param child Pointer to the child Component.
 */
void Component::addChild(Component *parent, Component *child)
{
  // Link parent to child.
  child->parent = parent;
  // Set flag for child.
  child->hasParent = true; 
  // Link child to parent.
  this->children.push_back(child); 
}

/**
 * @brief Set the Translation matrix for the Component.
 * 
 * @param x Units to shift by on the X-axis.
 * @param y Units to shift by on the Y-axis.
 * @param z Units to shift by on the Z-axis.
 */
void Component::setTranslate(float x, float y, float z)
{
  this->translate = this->translate * Transformation::translate(x, y, z);
}

/**
 * @brief Set the Scale matrix for the Component.
 * 
 * @param x Units to scale by on the X-axis.
 * @param y Units to scale by on the Y-axis.
 * @param z Units to scale by on the Z-axis.
 */
void Component::setScale(float x, float y, float z)
{
  this->scale = this->scale * Transformation::scale(x, y, z);
}

/**
 * @brief Set the Rotate matrix for the Component.
 * 
 * @param x Units to rotate by on the X-axis.
 * @param y Units to rotate by on the Y-axis.
 * @param z Units to rotate by on the Z-axis.
 */
void Component::setRotate(float x, float y, float z)
{
  this->rotate = this->rotate * Transformation::rotate(x, y, z);
}

/**
 * @brief Get the Rotation matrix for the Component.
 * 
 * @return Product of all parent rotations and the curent rotation.
 */
mat4 Component::getRotation(void)
{
  // If the Component has a parent, get all the parent transformations.
  // Then return the components transformation times the parent transformations.
  if (hasParent)
  {
    mat4 parentMatrix = parent->getRotation();
    return parentMatrix * this->rotate; 
  }
  // Otherwise, return only the components transformation.
  else
  {
    return this->rotate;
  }
}

/**
 * @brief Get the Scale matrix for the Component.
 * 
 * @return Product of all parent scales and the curent scales. 
 */
mat4 Component::getScale(void)
{
  // If the Component has a parent, get all the parent transformations.
  // Then return the components transformation times the parent transformations.
  if (hasParent)
  {
    mat4 parentMatrix = parent->getScale();
    return parentMatrix * this->scale; 
  }
  // Otherwise, return only the components transformation.
  else
  {
    return this->scale;
  }
}

/**
 * @brief Get the net transformation matrix for the Component.
 * 
 * @return Product of all parent transformations and the curent transformation.
 */
mat4 Component::getTransformation(void)
{
  // If the Component has a parent, get all the parent transformations.
  // Then return the components transformation times the parent transformations.
  if (hasParent)
  {
    mat4 parentMatrix = parent->getTransformation();
    return parentMatrix * (this->translate * this->scale * this->rotate);
  }
  // Otherwise, return only the components transformation.
  else
  {
    return this->translate * this->scale * this->rotate;
  }
}

/**
 * @brief Loads the component into the GPU and then draws.
 * 
 */
void Component::display(void)
{
  // Build and link vao buffer
  GLuint vao;
  glGenVertexArrays(1, &vao);
  glBindVertexArray(vao);
  // Get the data for the given shape.
  vector<float> shapeData = shape.getData();
  // Bind buffer, link data.
  GLuint buffer = 0;
  glGenBuffers(1, &buffer);
  glBindBuffer(GL_ARRAY_BUFFER, buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * shapeData.size(), &shapeData[0], GL_STATIC_DRAW);
  // Align position.
  GLuint pos = 0;
  glEnableVertexAttribArray(pos);
  glBindBuffer(GL_ARRAY_BUFFER, buffer);
  glVertexAttribPointer(pos, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 8, (GLvoid *)(sizeof(GLfloat) * 0));
  // Align surface normals.
  GLuint normal = 1;
  glEnableVertexAttribArray(normal);
  glBindBuffer(GL_ARRAY_BUFFER, buffer);
  glVertexAttribPointer(normal, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 8, (GLvoid *)(sizeof(GLfloat) * 4));
  // Set view
  mat4 view = Transformation::perspectiveProjection(1, 100, 90);
  mat4 transformation = view * this->getTransformation();
  GLuint childTransformationID = glGetUniformLocation(this->programID, "mVertexTransformation");
  glUniformMatrix4fv(childTransformationID, 1, GL_TRUE, &transformation[0][0]);
  // Link rotation mat4.
  mat4 normalTransformation = this->getScale() * this->getRotation();
  GLuint normalTransID = glGetUniformLocation(this->programID, "mNormalTransformation");
  glUniformMatrix4fv(normalTransID, 1, GL_TRUE, &normalTransformation[0][0]);
  // Link color vec4.
  GLuint colorID = glGetUniformLocation(this->programID, "vColor");
  glUniform4fv(colorID, 1, &this->color.get().front());
  // Draw data.
  glDrawArrays(GL_TRIANGLES, 0, (shapeData.size() / 8.0));
  // If the Component has children, draw them.
  if (children.size())
  {
    for (int i = 0; i < children.size(); i++)
    {
      children[i]->display();
    }
  }
  // Delete the vertex array.
  glDeleteVertexArrays(1, &vao);
  // Delete the buffer.
  glDeleteBuffers(1, &buffer);
}



