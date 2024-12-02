/*
 Programmer: Andrew Januszko      Date of Creation: 02/28/19
 Instructor: Chen Huo             Course: ENGR 120 - 03

 Find the greatest common factor of two numbers.
*/
#include <stdio.h> /* printf, scanf definitions. */
#include <stdlib.h> /* abs() definition. */

int main(void){
  int m, /* Holds the first number. */
      n, /* Holds the second number. */
      gdc; /* Holds the greatest common factor. */
  /* Ask the user to input two numbers, then store them. */
  printf("Enter two numbers (m n): ");
  scanf("%d %d", &m, &n);
  /* Convert both numbers to positive integers. */
  m = abs(m);
  n = abs(n);
  /* While i is less than both m and n. */
  for(int i = 1; i <= m && i <= n; i++){
    /* If m mod i and n mod i equal 0. */
    if((m % i == 0) && (n % i == 0)){
       /* Store as a common factor. */
       gdc = i;
    }
  }
  /* Print the greatest common factor. */
  printf("%d\n", gdc);
  /* Return 0. */
  return 0;
}
