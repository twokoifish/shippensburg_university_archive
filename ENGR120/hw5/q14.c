/*
 Programmer: Andrew Januszko        Date of Creation: 03/11/18
 Instructor: Chen Huo               Course: ENGR 120 - 03

 Brothers's and Knox's Approximation of e.
*/
#include <stdio.h> /* printf() definition. */
#include <math.h> /* pow() definition. */
#include <stdlib.h> /* fabs() definition. */
#define EPSILON 0.000001 /* Holds the absolute difference for the expression. */
#define EXP_ONE 1.0 /* Holds the constant value for the exp() function. */

/* Calculates the approximation of e based on the x value passed in. */
void approximation_of_e(double x, double *approximate_E){
  /* Calcuclate the base value to be approximated to the 'x' power. */
  double base = ((2 * x) + 1) / ((2 * x) - 1);
  /* Approximate 'e' by putting 'base' to the power of 'x'. */
  *approximate_E = pow(base, x);
}

/* Prints the approximation of e until the conditon is no longer satisfied. */
void print_approximation(){
  /* Holds the approximation of e. */
  double approximate_E;
  /* Holds the value of 'x'. */
  double x = 1;
  /* Holds 'e' to the first power. */
  double e_to_x = exp(EXP_ONE);
  /* Calls the 'approximation_of_e' function and passes in 'x' and returns 'approximate_E'. */
  approximation_of_e(x,&approximate_E);
  /* Calculates the difference between 'approximate_E' and 'e_to_x'. */
  double difference = fabs(approximate_E - e_to_x);
  /* While the difference is greater than or equal to epsilon. */
  while(difference >= EPSILON){
    /* Increment 'x'. */
    ++x;
    /* Calls the 'approximation_of_e' function and passes in 'x' and returns 'approximate_E'. */
    approximation_of_e(x,&approximate_E);
    /* Calculates the difference between 'approximate_E' and 'e_to_x'. */
    difference = fabs(approximate_E - e_to_x);
  }
  /* Print where they converged, and the value they converged at. */
  printf("Converged at x = %.0f, %.7f, exp(%.1f) = %.7f\n", x, approximate_E, EXP_ONE, e_to_x);
}

int main(void){
  /* Calls the 'print_approximation' function. */
  print_approximation();
  /* Returns zero to satisfy the integer return value for the main function. */
  return 0;
}