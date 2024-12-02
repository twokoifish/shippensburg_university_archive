/*
 Programmer: Andrew Januszko                   Date Completed: February 1, 2019
 Instructor: Chen Huo                          Class: ENGR 120-03

 A program that that estimated water saving and cost of replacement for toilets.
*/
#include <stdio.h> /* printf, scanf definitions. */

int main (void){
  
  double population, /* The amount of people in the community. */
         net, /* the total net savings in liters. */
         new, /* the amount of liters used by the new toilets. */
         existing, /* the amount of liters used by the existing toilets. */
         cost; /* the cost to install the new toilets. */
  
  /* Ask for the population size, then store user input. */
  printf("Enter population: ");
  scanf("%lf", &population);

  /* Calulate the net savings and cost of installation. */
  population = population / 3;
  cost = population * 150;
  existing = population * (15 * 14);
  new = population * (2 * 14);
  net = existing - new;

  /* Return the water savings and the cost of replacement. */
  printf("Estimated water savings: %.1fL\n", net);
  printf("Cost to replace: $%.2f\n", cost);

  /* Return. */
  return (0);
}
