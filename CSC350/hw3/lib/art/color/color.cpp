#include "color.h"

vector<Color> ColorManager::getPalette(void)
{
  vector<Color> palette;
  Color yellow1 = {0.97f, 0.84f, 0.09f, 1.00f}; // lightest yellow, changed to d# to be compatable with cmath.
  palette.push_back(yellow1);
  Color yellow2 = {0.87f, 0.65f, 0.03f, 1.00f}; // middle yellow, changed to d# to be compatable with cmath.
  palette.push_back(yellow2);
  Color yellow3 = {0.56f, 0.40f, 0.06f, 1.00f}; // darkest yellow, changed to d# to be compatable with cmath.
  palette.push_back(yellow3);
  Color tan1 = {0.53f, 0.50f, 0.34f, 1.00f}; // tan, only used along beak
  palette.push_back(tan1);
  Color blue1 = {0.09f, 0.56f, 0.97f, 1.00f}; // lightest blue
  palette.push_back(blue1);
  Color blue2 = {0.06f, 0.37f, 0.81f, 1.00f}; // middle blue
  palette.push_back(blue2);
  Color blue3 = {0.00f, 0.25f, 0.62f, 1.00f}; // darkest blue
  palette.push_back(blue3);
  Color white1 = {0.94f, 0.94f, 0.94f, 1.00f}; // lightest white
  palette.push_back(white1);
  Color white2 = {0.81f, 0.81f, 0.81f, 1.00f}; // middle white
  palette.push_back(white2);
  Color white3 = {0.59f, 0.62f, 0.62f, 1.00f}; // darkest white
  palette.push_back(white3);
  Color black1 = {0.28f, 0.31f, 0.34f, 1.00f}; // lightest black, almost grey
  palette.push_back(black1);
  Color black2 = {0.21f, 0.25f, 0.31f, 1.00f}; // purpleish black
  palette.push_back(black2);
  Color black3 = {0.18f, 0.21f, 0.25f, 1.00f}; // darker purple black
  palette.push_back(black3);
  Color black4 = {0.15f, 0.18f, 0.21f, 1.00f}; // darker black
  palette.push_back(black4);
  Color orange1 = {1.00f, 0.89f, 0.54f, 1.00f}; // orange background.
  palette.push_back(orange1);
  return palette;
}