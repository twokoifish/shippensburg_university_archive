/*
 Programmer: Andrew Januszko            Date of Creation: 02/15/19
 Instructor: Chen Huo                   Course: ENGR 120 - 03
 
 A program that rounds to two decimal places.
 */
#include <stdio.h> /* printf, scanf definitions. */

int main(void){
  double number; /* Holds the number being rounded. */
    
  /* Print the title of the program. */
  printf("***** Round to two decimal places *****\n");
    
  /* Ask the user for a number to round, then store the input. */
  printf("Enter the number: ");
  scanf("%lf", &number);
    
  /* Prints the rounded number. */
  printf("Rounded %.2f0000\n", number);
    
  /* Return 0. */
  return(0);
}
