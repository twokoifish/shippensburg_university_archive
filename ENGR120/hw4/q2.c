/*
 Programmer: Andrew Januszko        Date of Creation: 02/28/19
 Instructor: Chen Huo               Course: ENGR 120 - 03
 
 Check to see if a number is evenly divisible by 9.
*/
#include <stdio.h> /* printf(), getchar() definition. */

int main(void){
  
  char digit; /* Holds each individual number in the integer. */
  int sum = 0; /* The sum of the numbers in the integer. */
    
  /* Ask the user for input. */
  printf("Enter a number: ");
  /* use getchar to store the input. */
  digit = getchar();
  /* Run this loop until the enter key is stored. */
  do{
    sum += ((int)digit - (int)'0'); /* convert digit to an int and add it to the sum. */
    printf("d = %c, sum = %d\n", digit, sum); /* print the digit and the sum. */
    digit = getchar(); /* use getchar to store the input. */
  }while(digit != 10);
  /* If the sum is divisible by 9. */
  if((sum % 9) == 0){
    /* Print the number and that it is divisible. */
    printf("is divisible by 9\n");
  /* If the sum not is divisible by 9. */
  }else{
    /* Print the number and that it is not divisible. */
    printf("is not divisible by 9\n");
  }
  /* Return 0. */
  return 0;
}
