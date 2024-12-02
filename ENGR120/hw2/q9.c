/*
 Programmer: Andrew Januszko            Date of Creation: 02/16/19
 Instructor: Chen Huo                   Course: ENGR 120 - 03
 
 Calculate the number of flu cases after x number of days.
 */
#include <stdio.h> /* printf, scanf definitions. */
#include <math.h> /* exp() definition. */

int cases(int x); /* Define the 'cases' function. */

int main(void){
  int dayNumber; /* Holds the day number. */
    
  /* Print the title of the program. */
  printf("FLU EPIDEMIC PREDICTIONS BASED ON ELAPSED DAYS SINCE FIRST CASE REPORT\n");
    
  /* Repeats the actions below 3 times. */
  for(int i = 0; i < 3; i++){
    /* Print the prompt and store user input. */
    printf("Enter day number>> ");
    scanf("%d", &dayNumber);
        
    /* Print the day number and the number of cases. */
    printf("By day %d, model predicts %d cases total.\n", dayNumber, cases(dayNumber));
  }
    
  /* Return 0. */
  return(0);
}

/* The function that calculates the number of cases. */
int cases(int x){
 return (40000 / (1 + 39999 * exp(-0.24681 * x)));
}
