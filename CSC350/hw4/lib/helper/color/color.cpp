#include "color.h"

Color::Color(float redVal, float greenVal, float blueVal)
{
  data = {
      redVal,
      greenVal,
      blueVal,
      1.00f,
  };
}

Color::Color(float redVal, float greenVal, float blueVal, float alphaVal)
{
  data = {
      redVal,
      greenVal,
      blueVal,
      alphaVal,
  };
}

vector<float> Color::get(void)
{
  return data;
}