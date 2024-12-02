#include <stdio.h> /* printf, scanf definitions. */
#include <math.h> /* pow definition. */

/* Evaluates the value of the polynomial. */
void eval_poly(const double coeff[], int degree, double x) {
  /* Holds the value of the polynomial. */
  double polynomial = 0;
  /* Assigns the first value to the polynomial. */
  polynomial = coeff[0];
  /* Calulate the value of the polynomial. */
  for(int i = 1; i < degree + 1; i++) {
    polynomial += (coeff[i] * pow(x, i));
  }
  /* Print the value of the polynomials. */
  printf("Value: %f\n", polynomial);
}

/* Gets the degree and values of the polynomial. */
void get_poly(void) {
  int degree; /* Holds the degree. */
  /* Ask the user to enter a degree and store it. */
  printf("Enter the degree [1,8]: ");
  scanf("%d", &degree);
  /* Declare the coeff array with a length one
     more longer than the actual degree.
  */
  double coeff[degree + 1], x;
  /* If the number entered is valid. */
  switch (degree) {
    case 1 ... 8:
      /* Fetch the degree values. */
      for(int i = degree; i >= 0; i--) {
        printf("Enter A[%d]: ", i);
        scanf("%lf", &coeff[i]);
      }
      /* Ask for the value of x and store it. */
      printf("Enter x: ");
      scanf("%lf", &x);
      /* Calls the 'eval_poly' function. */
      eval_poly(coeff, degree, x);
      /* Break the loop. */
      break;
    default:
      /* State that the degree is invalid. */
      printf("Invalid degree!\n"
             "Please enter a valid degree between 1 and 8.\n");
      /* Call the 'get_poly' function to start over. */
      get_poly();
      /* Break the loop. */
      break;
  }
}

int main(void) {
  /* Calls the 'get_poly' function. */
  get_poly();
  /* Return 0. */
  return 0;
}