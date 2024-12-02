/*
  Programmer: Andrew Januszko   Date of Creation: 02/19/19
  Instructor: Chen Huo          Course: ENGR 120 - 03

  A program that calculates BMI from height and weight.
*/
#include <stdio.h> /* printf, scanf definitions. */
#include <math.h> /* pow() definition. */

/* Definition for the 'bmi' function. */
void bmi(double weight, double height);

int main(void){
  double weight, /* Holds the weight in pounds. */
         height; /* Holds the height in inches. */

  /* Ask the user for weight in pounds, then store it. */
  printf("Enter weight in pounds: ");
  scanf("%lf", &weight);

  /* Ask the user for height in inches, then store it. */
  printf("Enter height in inches: ");
  scanf("%lf", &height);

  /* Calls the bmi function. */
  bmi(weight, height);

  /* Return 0. */
  return(0);
}

/* Calulates the bmi, and states their weight status. */
void bmi(double weight, double height){
  /* Calulate the bmi. */
  double bmi = ((703 * weight) / pow(height, 2));

  /* Print the bmi and the weight status. */
  /* If bmi is under 18.5, underweight. */
  if(bmi < 18.5){
    printf("%.1f Underweight.\n", bmi);
      
  /* If bmi is above 18.5 and below 24.9, normal. */
  } else if(bmi >= 18.5 && bmi <= 24.9){
    printf("%.1f Normal.\n", bmi);
      
  /* If bmi is above 25.0 and below 29.9, overweight. */
  } else if(bmi >= 25.0 && bmi < 29.9){
    printf("%.1f Overweight.\n", bmi);
      
  /* If bmi is above 29.9, obese. */
  } else if(bmi > 29.9){
    printf("%.1f Obese.\n", bmi);
  }
}
