/*
 Programmer: Andrew Januszko                   Date Completed: February 1, 2019
 Instructor: Chen Huo                          Class: ENGR 120-03

 A program that estimates temperture in a freezer since a power faliure.
*/
#include <stdio.h> /* printf, scanf definitions. */
#include <math.h> /* pow(b,p) definition. */
#define HOUR_TO_RATIO (1.0/60.0) /* define the constant for an hour. */

int main (void){
  
  double hours, /* holds the hours since power failure. */
         minutes, /* holds the minutes since power failure. */
         temperature; /* holds the temperature 't' hours after power failure. */

  /* Ask for the time since power failure, then get user input. */
  printf("Enter the time since power failure (HH MM): ");
  scanf("%lf%lf", &hours, &minutes);

  /* Convert minutes to hours then add it to the hours. */
  hours += (minutes * HOUR_TO_RATIO);

  /* Calulate the temperature of the freezer. */
  temperature = ((4 * pow(hours, 2)) / (hours + 2)) - 20;

  /* Return the temperature of the freezer. */
  printf("Temperature is: %.1f\n", temperature); 
  
  /* Return. */
  return(0);
}
