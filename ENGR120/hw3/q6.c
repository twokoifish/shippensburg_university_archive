/*
 Programmer: Andrew Januszko        Date of Creation: 02/21/19
 Instructor: Chen Huo               Course: ENGR 120 - 03
 
 A program that tells you the location of a point.
*/
#include <stdio.h> /* printf, scanf definitions. */

/* Definition for 'coordinates' function. */
void coordinates(double x, double y);

int main(void){
  double x, /* Holds the x value. */
         y; /* Holds the y value. */

  /* Ask the user for an x and y coordinate. */
  printf("Please enter the coordinates, separate by a comma: ");
    
  /* Store the variables as x, y. */
  scanf("%lf,  %lf", &x, &y);
    
  /* Call the 'coordinates' function, and pass x and y in. */
  coordinates(x, y);
    
  /* Return 0. */
  return 0;
}

/* Determine the position of the point x, y. */
void coordinates(double x, double y){
  /* If x is positive. */
  if(x > 0.0){
    /* If y is positive. */
    if(y > 0.0){
      /* Print the location. */
      printf("The point (%.2f, %.2f) lies in quadrant I.\n", x, y);
    /* If y is 0. */
    }else if(y == 0.0){
      /* Print the location. */
      printf("The point (%.2f, %.2f) lies on the x_axis.\n", x, y);
    /* If y is negative. */
    }else{
      /* Print the location. */
      printf("The point (%.2f, %.2f) lies in quadrant IV.\n", x, y);
    }
  /* If x is 0. */
  }else if(x == 0.0){
    /* If y is positive. */
    if(y > 0.0){
      /* Print the location. */
      printf("The point (%.2f, %.2f) lies on the y-axis.\n", x, y);
    /* If y is 0. */
    }else if(y == 0.0){
      /* Print the location. */
      printf("The point (%.2f, %.2f) lies at the origin.\n", x, y);
    /* If y is negative. */
    }else{
      /* Print the location. */
      printf("The point (%.2f, %.2f) lies on the y-axis.\n", x, y);
    }
  /* If x is negative. */
  }else{
    /* If y is positive. */
    if(y > 0.0){
      /* Print the location. */
      printf("The point (%.2f, %.2f) lies in quadrant II.\n", x, y);
    /* If y is 0. */
    }else if(y == 0.0){
      /* Print the location. */
      printf("The point (%.2f, %.2f) lies on the x_axis.\n", x, y);
    /* If y is negative. */
    }else{
      /* Print the location. */
      printf("The point (%.2f, %.2f) lies in quadrant III.\n", x, y);
    }
  }
}
