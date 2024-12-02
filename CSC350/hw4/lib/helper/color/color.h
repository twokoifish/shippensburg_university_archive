#ifndef COLOR_H
#define COLOR_H

#include <iostream>
#include <vector>

using namespace std;


class Color
{
  private:
    vector<float> data;
  public:
    Color(float redVal, float greenVal, float blueVal);
    Color(float redVal, float greenVal, float blueVal, float alphaVal);
    vector<float> get(void);
};

#endif // COLOR_H