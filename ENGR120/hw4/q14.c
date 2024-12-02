/*
 Programmer: Andrew Januszko        Date of Creation: 03/03/19
 Instructor: Chen Huo               Course: ENGR 120 - 03

 Calculate the half - life of an ammount of Cobalt 60.
*/
#include <stdio.h> /* printf, scanf definition. */
#include <math.h> /* pow(), M_E definition. */

/* Definition for the 'halflife' function. */
void halflife(double amount);

int main(void){
  double amount; /* Holds the amount of Co60. */

  /* Print the title, and ask the user to input an amount of Co60. */
  printf("*******************\n");
  printf("Half-Life Co-60\n");
  printf("*******************\n");
  printf("Enter amount of Co-60: ");
  /* Store the amount of Co60. */
  scanf("%lf", &amount);

  /* Call 'halflife' function. */
  halflife(amount);

  /* Return 0. */
  return 0;
}

/* Calculate the remaining amount of Co60. */
void halflife(double amount){
  double r; /* Gholds the remaining Co60. */
  double C = pow(M_E, -0.693); /* The math in the pow(). */
  /* Print the year and the amount. */
  printf("Year\t\tAmount\n");
  /* While i is less than the half - life of Co60. */
  for(double i = 1; i < 5.272; i++){
    /* Calculate the remaining Co60. */
    r = amount * pow(C, i/5.272);
    /* Print the # of years and the remaining Co60. */
    printf("%.0f\t\t%.6f\n", i, r);
  }
}
