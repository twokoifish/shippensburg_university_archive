/*
 Programmer: Andrew Januszko                   Date Completed: February 1, 2019
 Instructor: Chen Huo                          Class: ENGR 120-03

 A program that calculated pythagorean triples.
*/
#include <stdio.h> /* printf, scanf definitions. */
#include <math.h> /* pow(b,p) definitions. */

int main (void){

  double n, /* one length of the triangle. */
         m, /* another length of the triangle. */
         sideOne, /* side one of the triangle. */
         sideTwo, /* side two of the triangle. */
         hypotenuse; /* hypotenuse of the triangle. */

  /* Ask for two lenghts of the triangle, then gather user input. */
  printf("Enter m and n (m > n): ");
  scanf("%lf %lf", &m, &n);

  /* Calulate the sides and the hypotenuse. */
  sideOne = pow(m,2.0) - pow(n,2.0);
  sideTwo = 2 * (m*n);
  hypotenuse = pow(m,2.0) + pow(n,2.0);

  /* Return the pythagorean triples. */
  printf("%.f %.f %.f\n", sideOne, sideTwo, hypotenuse);

  /* Return. */
  return (0);
}
