/*
 Programmer: Andrew Januszko                   Date Completed: February 1, 2019
 Instructor: Chen Huo                          Class: ENGR 120-03

 A program that calulates the rate to infuse liquid at.
*/
#include <stdio.h> /* printf, scanf definitions. */
#define HOUR_TO_RATIO (1.0/60.0) /* define the constant for an hour. */

int main (void){

  double volume, /* the amount of liquid to infuse. */
         minutes, /* the time over which to infuse it. */
         rate; /* the estimated rate to infuse at. */

  /* Ask the user what the volume of the liquid is, then store user input. */
  printf("Volumne to be infused (ml) => ");
  scanf("%lf", &volume);

  /* Ask the user over how many minutes to infuse the liquid at, the store user input. */
  printf("Minutes over which to infuse => ");
  scanf("%lf", &minutes);

  /* Calulate the rate to infuse at. */
  rate = volume / (minutes * HOUR_TO_RATIO) ;

  /* Return the volume to be infused and the rate to infuse it at. */
  printf("\n");
  printf("VTBI: %.f ml\n", volume);
  printf("Rate: %.1f ml/hr\n", rate);

  /* Return. */
  return (0);
}
