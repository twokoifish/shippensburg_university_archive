/*
 Programmer: Andrew Januszko            Date of Creation: 02/15/19
 Instructor: Chen Huo                   Course: ENGR 120 - 03

 A program that calculates the fractoral value of N.
*/
#include <stdio.h> /* printf, scanf definitions. */
#include <math.h> /* pow(), exp(), M_PI, and sqrt() definitions. */

/* Definition for the approximate function. */
double approximate(double n);

int main(void){
  double n; /* The number to be approximated. */
    
  /* Print the title. */
  printf("****** Approximate N! *******\n");
    
  /* Ask the user for the number, then store it. */
  printf("Enter n: ");
  scanf("%lf", &n);
    
  /* Return the factoral. */
  printf("%.f! equals approximately %.6f\n", n, approximate(n));
    
  /* Return 0. */
  return(0);
}

/* Calculate the factoral. */
double approximate(double n){
  /* Create the number to go under the root. */
  double uRoot = ((2 * n) + (1.0/3.0)) * M_PI;
  
  /* Calculate and return the factoral. */
  return (pow(n, n) * exp(-n) * sqrt(uRoot));
}
