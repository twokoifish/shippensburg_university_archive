#ifndef COLOR_H
#define COLOR_H

#include <iostream>
#include <vector>

using namespace std;

typedef struct COLOR_T
{
  float red;
  float green;
  float blue;
  float alpha;
} Color;

class ColorManager
{
  public:
  static vector<Color> getPalette(void);
};

#endif