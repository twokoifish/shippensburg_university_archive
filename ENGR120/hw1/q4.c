/*
 Programmer: Andrew Januszko                   Date Completed: February 1, 2019
 Instructor: Chen Huo                          Class: ENGR 120-03

 A program that converts fahrenheit to kelvin.
*/
#include <stdio.h> /* printf, scanf definitions. */

int main(void){
  
  int fahrenheit; /* holds a int  value in degrees fahrenheit. */
  double kelvin; /* holds a double value in degrees kelvin. */

  /* Ask for the temperature in fahrenheit, then store user input. */
  printf("Enter a temperature in degrees Fahrenheit: ");
  scanf("%d", &fahrenheit);

  /* Convert from fahrenheit to kelvin. */
  kelvin = ((5.0 / 9.0) * (fahrenheit - 32)) + 273.15;
  
  /* Return the original degrees in fahrenheit, then in degrees kelvin. */
  printf("%d degrees in Fahrenheit is %.2f degrees in Kelvin.\n", fahrenheit, kelvin);

  /* Return. */
  return (0);
}
