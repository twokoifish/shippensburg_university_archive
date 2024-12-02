/*
 Programmer: Andrew Januszko            Date of Creation: 02/07/19
 Instructor: Chen Huo                   Course: ENGR 120 - 03

 A program that calulates payments, based on the price of a car.
*/
#include <stdio.h> /* printf, scanf definitions. */
#include <math.h> /* pow definitions. */
#define YEARS_MONTHS (1.0/12.0) /* Defines the conversion of years to months. */
#define PERCENTAGE (1.0/100.0) /* Defines the conversion to a percentage. */

/* Definition for the borrow function. */
double borrow(double purchase, double down);

/* Definition for the payment function. */
double payment(double P, double i, double n);


int main(void){
  double purchasePrice, /* The original price of the car. */
         downPayment, /* The down payment on the car. */
         annualInterestRate, /* The annual (yearly) interest rate. */
         months; /* The number of months to pay off the car. */
    
  /* Ask for purchase price, then store user input. */
  printf("Enter purchase price: ");
  scanf("%lf", &purchasePrice);
    
  /* Ask for down payment, then store user input. */
  printf("Enter down payment: ");
  scanf("%lf", &downPayment);
    
  /* Ask for the interest rate, then store user input. */
  printf("Enter annual interest rate (e.g. 5.5): ");
  scanf("%lf", &annualInterestRate);
    
  /* Ask for the months to span over, then store user input. */
  printf("Enter number of month: ");
  scanf("%lf", &months);
    
  /* Print the borrow amount. */
  printf("The amount you borrow will be $%.2f\n", borrow(purchasePrice, downPayment));
    
  /* Print the monthly payment. */
  printf("Your payment will be $%.2f\n", payment(borrow(purchasePrice, downPayment), annualInterestRate, months));
    
  /* Return 0. */
  return(0);
}
/* Calulate the borrow amount for the car. */
double borrow(double purchase, double down){
  /* Calculate the amount to be borrowed. */
  return purchase - down;
}

/* Calulate the payment rate for the car. */
double payment(double P, double i, double n){
  /* Convert the annual interest rate into a monthly percentage. */
  i = (i * YEARS_MONTHS) * PERCENTAGE;

  /* Calulate the payment rate. */
  return (i * P) / (1 - pow(1 + i, -n));
}
