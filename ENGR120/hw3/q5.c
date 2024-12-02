/*
 Programmer: Andrew Januszko        Date of Creaton: 02/21/19
 Instructor: Chen Huo               Course: ENGR 120 - 03
 
 A program that states the damage from a earthquake.
*/
#include <stdio.h> /* printf, scanf definitions. */

/* Definition for the 'richter' function. */
void richter(double number);

int main(void){
  double number; /* Holds the number on the richter scale. */
    
  /* Ask the user for a number on the richter scale. */
  printf("Enter the number on the Richter scale>  ");

  /* Store the user's number in the number variable. */
  scanf("%lf", &number);
    
  /* Call the 'richter' function and pass number into it. */
  richter(number);
    
  /* Return 0. */
  return(0);
}

/* Determines damage from the number passed in. */
void richter(double number){
  /* If number < 5. */
  if(number < 5.0){
    /* Print estimated damage. */
    printf("[%.1f] -- Little or no damage.\n", number);
  /* If 5 < number < 5.5 */
  }else if(number >= 5.0 && number < 5.5){
    /* Print estimated damage. */
    printf("[%.1f] -- Some damage.\n", number);
  /* If 5.5 < number < 6.5 */
  }else if(number >= 5.5 && number < 6.5){
    /* Print estimated damage. */
    printf("[%.1f] -- Serious damage: \n       walls may crack or fall.\n", number);
  /* If 6.5 < number < 7.5 */
  }else if(number >= 6.5 && number < 7.5){
    /* Print estimated damage. */
    printf("[%.1f] -- Disaster: \n\thouses and buildings may collapse.\n", number);
  /* If 7.5 < number */
  }else if(number >= 7.5){
    /* Print estimated damage. */
    printf("[%.1f] -- Catastrophe:  \n       most buildings destroyed.\n", number);
  }
}
