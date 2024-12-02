/*
 Programmer: Andrew Januszko            Date of Creation: 02/17/19
 Instructor: Chen Huo                   Course: ENGR 120 - 03
 
 Calculate the ratio of speeds of six speed gearbox.
*/
#include <stdio.h> /* printf, scanf definitions. */
#include <math.h> /* pow() definition. */

/* Definition for the 'speeds_ratio' function. */
double speeds_ratio(double M, double m);

int main(void){
  double M, /* Holds the maximum speed. */
         m; /* Holds the minimum speed. */
  /* Print the title. */
  printf("**** Ratio of six-speed gearbox ****\n");

  /* Ask for the minimum speed, then store it. */
  printf("Enter minimum speed: ");
  scanf("%lf", &m);
    
  /* Ask for the maximum speed, then store it. */
  printf("Enter maximum speed: ");
  scanf("%lf", &M);

  /* Print the minimum and maximum speed, and its sucessive speeds. */
  printf("The ratio between successive speeds of a six-speed gearbox with maximum speed %.1f rpm and minimum speed %.1f rpm is %.6f\n", m, M, speeds_ratio(M, m));
    
  /* Return 0. */
  return(0);
}

/* Calculate the successive speeds ratio for the gearbox. */
double speeds_ratio(double M, double m){
  return pow(M/m, 1.0/5.0);
}
