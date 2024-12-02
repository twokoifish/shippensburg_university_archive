/*
 Programmer: Andrew Januszko    Date of Creation: 02/25/19
 Instructor: Chen Huo           Course: ENGR 120 - 03

 A program to see if a number is divisible by 9.
*/
#include <stdio.h> /* printf, scanf definitions. */

int main(void){
  int n, /* Holds the original value of n. */
      nFunctional, /* Holds the current value of n. */
      digit, /* The temporary number removed from n. */
      sum; /* The sum of the integers in n. */

  /* Ask the user to enter an integer, then store it. */
  printf("Enter a number: ");
  scanf("%d", &n);

  /* Set the functional n equal to the value of the original n. */
  nFunctional = n;

  /* Keep dividing n while it is not equal to zero. */
  do{
    /* Get the end digit of the integer. */
    digit = nFunctional % 10;
    /* Add the integer to the sum. */
    sum += digit;
    /* Move to the next integer. */
    nFunctional = nFunctional / 10;
    /* Print the number we took off, and the sum of the numbers. */
    printf("d = %d, sum = %d\n", digit, sum);
  }while(nFunctional != 0);

  /* If the sum is not divisible by 9. */
  if(sum % 9 != 0){
    /* State that n is not divisible by 9. */
    printf("n = %d is not divisible by 9\n", n);
  /* If the sum is divisible by 9. */
  }else{
    /* State that n is divisible by 9. */
    printf("n = %d is divisible by 9\n", n);
  }

  /* Return 0. */
  return 0;
}
