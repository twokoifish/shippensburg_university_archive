/*
 Programmer: Andrew Januszko                   Date Completed: February 1, 2019
 Instructor: Chen Huo                          Class: ENGR 120-03

 A program that calulates milage reimbersement for a salesperson at a rate of $0.35 per mile.
*/
#include <stdio.h> /* printf, scanf definitions. */
#define RATE 0.35 /* The reimbursemnt rate per mile. */

int main(void){

  double beginning, /* holds the beginning odometer reading. */
         ending, /* holds the ending odometer reading. */
         netDistance, /* holds the difference from the beginning to the end reading. */
         reimbursement; /* holds the reimbursement amount. */
    
  /* Print the title of the program. */
  printf("MILEAGE REIMBURSEMENT CALCULATOR\n");

  /* Ask for the beginning odometer reading. */
  printf("Enter beginning odometer reading: ");
  scanf("%lf", &beginning); /* Scan for user input, store it into beginning (double). */

  /* Ask for the end odometer reading. */
  printf("Enter ending odometer reading: ");
  scanf("%lf", &ending); /* Scan for user input, store it into ending (double). */

  netDistance = ending - beginning; /* Calulate the net milage. */
  reimbursement = netDistance * RATE; /* Calulate the reimbursement. */

  /* Return the net distance traveled and the reimbursement rate. */
  printf("You traveled %.1f miles. At $%.2f per mile.\n", netDistance, RATE);
    
  /* Return the calulated reimbursement value. */
  printf("Your reimbursement is $%.2f.\n", reimbursement);

  /* Return 0. */
  return (0);
}
