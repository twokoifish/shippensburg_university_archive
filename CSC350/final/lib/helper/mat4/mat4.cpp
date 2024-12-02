/**
 * @file mat4.cpp
 * @author Andrew Januszko (aj8025@cs.ship.edu)
 * @brief Implementation of a mat4 class
 * @version 0.1
 * @date 2021-10-26
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "mat4.h"

/**
 * @brief Construct a new mat4 object
 * 
 */
mat4::mat4(void)
{
  for (int i = 0; i < MAT4_ROW_LEN; i++)
  {
    for (int j = 0; j < MAT4_COL_LEN; j++)
    {
      data[i][j] = 0;
    }
  }
}

/**
 * @brief Construct a new mat4 object
 * 
 * @param array Data used to build a mat4.
 */
mat4::mat4(float array[MAT4_ROW_LEN][MAT4_COL_LEN])
{
  for (int i = 0; i < MAT4_ROW_LEN; i++)
  {
    for (int j = 0; j < MAT4_COL_LEN; j++)
    {
      data[i][j] = array[i][j];
    }
  }
}

/**
 * @brief Returns a row of a mat4.
 * 
 * @param index The row of the mat4 to return.
 * @return A row of a mat4.
 */
float *mat4::operator[](int index)
{
  if (index < MAT4_ROW_LEN && index > -1)
  {
    return data[index];
  }
  else
  {
    cout << MAT4_IOB(index) << endl;
    exit(EXIT_FAILURE);
  }
}

/**
 * @brief Calculates the summation of two mat4.
 * 
 * @param operand The mat4 to apply.
 * @return The summation of two mat4. 
 */
mat4 mat4::operator+(mat4 operand)
{
  mat4 sum;
  for (int i = 0; i < MAT4_ROW_LEN; i++)
  {
    for (int j = 0; j < MAT4_COL_LEN; j++)
    {
      sum[i][j] = data[i][j] + operand[i][j];
    }
  }
  return sum;
}

/**
 * @brief Calculates the difference of two mat4.
 * 
 * @param operand The mat4 to apply.
 * @return The difference of two mat4. 
 */
mat4 mat4::operator-(mat4 operand)
{
  mat4 diff;
  for (int i = 0; i < MAT4_ROW_LEN; i++)
  {
    for (int j = 0; j < MAT4_COL_LEN; j++)
    {
      diff[i][j] = data[i][j] - operand[i][j];
    }
  }
  return diff;
}

/**
 * @brief Calculates the product of two mat4.
 * 
 * @param operand The mat4 to apply.
 * @return The product of two mat4. 
 */
mat4 mat4::operator*(mat4 operand)
{
  mat4 prod;
  for (int i = 0; i < MAT4_ROW_LEN; i++)
  {
    for (int j = 0; j < MAT4_COL_LEN; j++)
    {
      for (int k = 0; k < MAT4_ROW_LEN; k++)
      {
        prod[i][j] += data[i][k] * operand[k][j];
      }
    }
  }
  return prod;
}

/**
 * @brief Calculates the product of a scalar and mat4.
 * 
 * @param operand The scalar to apply.
 * @return The product of a scalar and mat4.
 */
mat4 mat4::operator*(float operand)
{
  mat4 prod;
  for (int i = 0; i < MAT4_ROW_LEN; i++)
  {
    for (int j = 0; j < MAT4_COL_LEN; j++)
    {
      prod[i][j] += data[i][j] * operand;
    }
  }
  return prod;
}

/**
 * @brief Prints the components of a mat4.
 * 
 */
void mat4::print(void)
{
  for (int i = 0; i < MAT4_ROW_LEN; i++)
  {
    cout << "[";
    for (int j = 0; j < MAT4_COL_LEN; j++)
    {
      if (j < MAT4_COL_LEN - 1)
      {
        cout << data[i][j] << ", ";
      }
      else
      {
        cout << data[i][j];
      }
    }
    cout << "]" << endl;
  }
}
