/*
 Programmer: Andrew Januszko            Date of Creation: 02/16/19
 Instructor: Chen Huo                   Course: ENGR 120 - 03
 
 Display the speed of a runner in in feet/second and meters/second.
*/
#include <stdio.h> /* printf, scanf definitions. */
/* Define the conversion of seconds to minutes, minutes to hours. */
#define TIME_CONVERSION (1.0/60.0)

/* Definition for the 'feetPerSecond' function. */
double feetPerSecond(double time);

/* Definition for the 'meterPerSecond' function. */
double meterPerSecond(double time);

int main(void){
  double minutes, /* Holds the minutes. */
         seconds, /* Holds the seconds. */
         hours; /* Holds the hours. */
  /* Print the title. */
  printf("***** Track Stars *****\n");
    
  /* Ask the user for the runners time. */
  printf("Enter time (minutes seconds): ");
    
  /* Store the user input. */
  scanf("%lf%lf", &minutes, &seconds);
    
  /* Convert from minutes to hours. */
  hours = ((minutes + (seconds * TIME_CONVERSION)) * TIME_CONVERSION);
    
  /* Call the 'feetPerSecond' and 'meterPerSecond' functions and print their output. */
  printf("%.2f feet/sec    %.2f meter/sec\n", feetPerSecond(hours), meterPerSecond(hours));
    
  /* Return 0. */
  return(0);
}

/* Calculate the speed in feet per second. */
double feetPerSecond(double time){
  double miles, /* Holds the miles. */
         feet; /* Holds the time in feet. */
  /* Convert to miles per hour. */
  miles = 1.0 / time;
    
  /* Convert to feet per second. */
  feet = ((miles * 5280) / 3600);
    
  /* Return in feet per second. */
  return feet;
}

/* Calculate the speed in meters per second. */
double meterPerSecond(double time){
  double feet, /* Holds the time in feet. */
         meters;  /* Holds the time in meters. */
  /* Fetch the time in feet per second. */
  feet = feetPerSecond(time);

  /* Convert from feet per second to meters per second. */
  meters = (feet * 0.3048);
    
  /* Return in meters per second. */
  return meters;
}
