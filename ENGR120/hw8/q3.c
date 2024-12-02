/*
  Programmer: Andrew Januszko         Date: 04/11/19
  Instructor: Chen Huo                Course: ENGR 120 - 03

  A program that gets scientific notation and some forms of
  the numbers.
*/
#include <stdio.h> /* printf() definitions. */
#include <math.h> /* pow() definitions. */
#include <string.h> /* strtok() definitions. */

/* Holds the data for a number in scientific notation. */
typedef struct {
  double mantissa;
  int exponent;
} sci_not_t;

/* Gets the numbers in scientific notation. */
void scan_sci(sci_not_t *number1, sci_not_t *number2);
/* Fetch the parts of the numbers. */
void fetch_parts(char buffer[1024], sci_not_t *number);
/* Print the numbers in scientific notation. */
void print_sci(sci_not_t number1, sci_not_t number2);
/* Prints the sum of the numbers. */
sci_not_t sci_sum(sci_not_t number1, sci_not_t number2);
/* Prints the difference of the numbers. */
sci_not_t sci_difference(sci_not_t number1, sci_not_t number2);
/* Prints the product of the numbers. */
sci_not_t sci_product(sci_not_t number1, sci_not_t number2);
/* Prints the quotients of the numbers. */
sci_not_t sci_quotient(sci_not_t number1, sci_not_t number2);

int main(void) {
  sci_not_t number1, /* Holds the first number. */
            number2, /* Holds the second number. */ 
            sum,  /* Holds the sum of the numbers. */
            diff,   /* Holds the difference of the numbers. */
            prod,   /* Holds the product of the numbers. */
            quot;  /* Holds the quotient of the numbers. */
  /* Calls scan_sci and gets the two numbers. */
  scan_sci(&number1, &number2);
  /* Calls print_sci and prints the two numbers. */
  print_sci(number1, number2);
  /* Calls sci_sum and gets the sum, then prints it. */
  sum = sci_sum(number1, number2);
  printf("Sum: %.6fe%d\n", sum.mantissa, sum.exponent);
  /* Calls sci_difference and gets the difference, then prints it. */
  diff = sci_difference(number1, number2);
  printf("Difference: %.6fe%d\n", diff.mantissa, diff.exponent);
  /* Calls sci_product and gets the product, then prints it. */
  prod = sci_product(number1, number2);
  printf("Product: %.6fe%d\n", prod.mantissa, prod.exponent);
  /* Calls sci_quotient and gets the quotient, then prints it. */
  quot = sci_quotient(number1, number2);
  printf("Quotient: %.6fe%d\n", quot.mantissa, quot.exponent);
}

/* Gets the numbers in scientific notation. */
void scan_sci(sci_not_t *number1, sci_not_t *number2) {
  char buffer[1024]; /* Holds the user input. */
  /* Get the first value, store it, then fetch its parts. */
  printf("Enter value1: ");
  fgets(buffer, sizeof(buffer), stdin);
  fetch_parts(buffer, number1);
  /* Get the second value, store it, then fetch its parts. */
  printf("Enter value2: ");
  fgets(buffer, sizeof(buffer), stdin);
  fetch_parts(buffer, number2);
}

/* Fetch the parts of the numbers. */
void fetch_parts(char buffer[1024], sci_not_t *number) {
  /* Get the first part of the number. */
  char *token = strtok(buffer, "e");
  if(token != NULL) {
    /* Store it. */
    sscanf(token, "%lf", &(*number).mantissa);
  }
  /* Get the second part of the number. */
  token = strtok(NULL, "\n");
  if(token != NULL) {
    /* Store it. */
    sscanf(token, "%d", &(*number).exponent);
  }
}

/* Print the numbers in scientific notation. */
void print_sci(sci_not_t number1, sci_not_t number2) {
  printf("\nValues input: %.6fe%d %.6fe%d\n", number1.mantissa, 
                                              number1.exponent, 
                                              number2.mantissa, 
                                              number2.exponent);
}

/* Calls sci_sum and gets the sum, then prints it. */
sci_not_t sci_sum(sci_not_t number1, sci_not_t number2) {
  /* Get the sum. */
  number1.mantissa = ((number1.mantissa * pow(10, number1.exponent)) +
                     (number2.mantissa * pow(10, number2.exponent))); 
  number1.exponent = 0;
  /* Divide till less than one, then store the exponent. */
  while(number1.mantissa >= 1.0) {
    number1.mantissa = number1.mantissa / 10.0;
    number1.exponent++;
  }
  /* Return the number. */
  return number1;
}

/* Prints the difference of the numbers. */
sci_not_t sci_difference(sci_not_t number1, sci_not_t number2) {
  /* Get the difference. */
  number1.mantissa = ((number1.mantissa * pow(10, number1.exponent)) -
                      (number2.mantissa * pow(10, number2.exponent)));
  number1.exponent = 0;
  /* Divide till less than one, then store the exponent. */
  while(number1.mantissa >= 1.0) {
    number1.mantissa = number1.mantissa / 10.0;
    number1.exponent++;
  }
  /* Return the number. */
  return number1;
}

/* Prints the product of the numbers. */
sci_not_t sci_product(sci_not_t number1, sci_not_t number2) {
  /* Get the product. */
  number1.mantissa = ((number1.mantissa * pow(10, number1.exponent)) *
                     (number2.mantissa * pow(10, number2.exponent)));
  number1.exponent = 0;
  /* Divide till less than one, then store the exponent. */
  while(number1.mantissa >= 1.0) {
    number1.mantissa = number1.mantissa / 10.0;
    number1.exponent++;
  }
  /* Return the number. */
  return number1;
}

/* Prints the quotients of the numbers. */
sci_not_t sci_quotient(sci_not_t number1, sci_not_t number2) {
  /* Get the quotient. */
  number1.mantissa = ((number1.mantissa * pow(10, number1.exponent)) / 
                     (number2.mantissa * pow(10, number2.exponent)));
  number1.exponent = 0;
  /* Divide till less than one, then store the exponent. */
  while(number1.mantissa >= 1.0) {
    number1.mantissa = number1.mantissa / 10.0;
    number1.exponent++;
  }
  /* Return the number. */
  return number1;
}