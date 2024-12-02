/*
 Programmer: Andrew Januszko        Date of Creation: 03/03/19
 Instructor: Chen Huo               Course: ENGR 120 - 02
 
 Approximate pi.
*/
#include <stdio.h> /* printf definition. */

int main(void){
  double base; /* Holds the base. */
  double pi; /* Holds pi. */
  /* Run when i is less than 100. */
  for(double i = 1.0; i < 100; i = i + 4){
    /* Add 1 over i. */
    base += (1.0/i);
    /* Subtract 1 over i plus 2. */
    base -= (1.0 / (i+2.0));
  }
  /* Generate pi. */
  pi = 4.0 * base;
  /* Print pi. */
  printf("Approx PI: %.6f\n", pi);
  /* Return 0. */
  return 0;
}
