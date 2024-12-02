/*
 Programmer: Andrew Januszko                   Date Completed: February 1, 2019
 Instructor: Chen Huo                          Class: ENGR 120-03

 A program the computes acceleration and time baed on the takeoff speed and dist ance.
*/
#include <stdio.h> /* printf, scanf definitions. */
#include <math.h> /* pow(b,p) definitions. */
#define KPM_TO_MPS 3.6 /* Conversion rate from kilometers per hour to meters per second. */

int main (void) {
  
  double velocity, /* holds kilometers per hour. */
         spatium, /* holds distance. */
         acceleration, /* holds acceleration. */
         time; /* holds time. */

  /* Ask for the takeoff speed and distance, then store user input. */
  printf("Enter takeoff speed (kph) and distance (m): ");
  scanf("%lf %lf", &velocity, &spatium);
    
  velocity = velocity/KPM_TO_MPS; /* Convert from kilometers per hour (k/h) to meters per second (m/s). */
//  time = spatium/velocity; /* Caulate the time from distance over time. */
//  acceleration = (1.0/2.0)*(velocity/time); /* Calculate the acceleration. */
//  time = (2 * spatium) / acceleration; /* Recalculate the time using the acceleration. */
//  time = sqrt(time); /* Recalculate the time. */
    
    velocity = velocity/KPM_TO_MPS;
    acceleration = pow(velocity, 2) / (2 * spatium);
    time = velocity / acceleration;

  /* Return the acceleration and time. */
  printf("Acceleration: %.2f m/s^2\n", acceleration);
  printf("Time: %.2f s\n", time);

  /* Return. */
  return (0);
}
