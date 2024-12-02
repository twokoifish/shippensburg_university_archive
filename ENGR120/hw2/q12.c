/*
 Programmer: Andrew Januszko            Date of Creation: 02/17/19
 Instructor: Chen Huo                   Course: ENGR 120 - 03
 
 Calculate the speed of sound based on the given temperature.
*/
#include <stdio.h> /* printf, scanf definitions. */
#include <math.h> /* sqrt() definition. */

/* Definition of the 'speedOfSound' function. */
double speedOfSound(double T);

int main(void){
  double temperature; /* Holds the temperature in Fahrenheit. */

  /* Ask the user for the temperature, the store it. */
  printf("Enter temp (F): ");
  scanf("%lf", &temperature);
    
  /* Return the temperature and the speed of sound in feet/second. */
  printf("Speed of sound @ T=%.2f is %.6f ft/s\n", temperature, speedOfSound(temperature));
    
  /* Return 0. */
  return(0);
}

/* Calculate the speed of sound using temperature. */
double speedOfSound(double T){
  /* The value under the squareroot. */
  double uRoot = (((5 * T) + (297)) / 247);

  /* Caluclate the speed of sound. */
  return (1086 * sqrt(uRoot));
}
