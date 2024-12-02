/*
 Programmer: Andrew Januszko      Date of Creation: 03/11/19
 Instructor: Chen Huo             Course: ENGR 120 - 03

 A function that calculates drag over increments of 5.
*/
#include <stdio.h> /* printf(), scanf() definition. */
#include <math.h> /* pow() definition. */
#define P 1.23 /* Holds the constant 'P'. */
#define VELOCITY_MAX 40.00 /* Holds the maximum velocity. */
#define VELOCITY_MIN 0.00 /* Holds the minumum velocity. */
#define INCREMENT 5.00 /* Holds the rate to increment the velocity at. */
#define STREAMLINE_MIN 0.2 /* Holds the streamline minimum value. */
#define STREAMLINE_MAX 0.5 /* Holds the streamline maximum value. */

/* Calculates the drag force with the provided data. */
void drag_force(double i, double A, double CD, double p, double *drag){
  /* Calculates drag using 'i', 'A', 'CD', 'P', and returns 'drag'. */
  *drag = (0.5)*CD*A*P*pow(i, 2);
}

/* Prints the drag calculated from 'drag_force'. */
void print_drag(double CD, double A){
  double drag; /* Holds the value of drag. */
  /* Print the header for the chart to be displayed. */
  printf("-------------------------------\n"
         "Velocity             Drag\n"
         "-------------------------------\n");
  /* Calculate 'drag' while 'i' is less than or equal to the maximum velocity. */
  for(double i = VELOCITY_MIN; i <= VELOCITY_MAX; i = i + INCREMENT){
    /* Calls the 'drag_force' function. */
    drag_force(i,A,CD,P,&drag);
    /* If the increment value is less than 10. */
    if(i < 10.00){
      /* Print with more spaces than the next. */
      printf("%.2f                %.3f\n", i, drag);
    /* If the increment value is greater than or equal to 10. */
    }else{
      /* Print with less spaces than the previous. */
      printf("%.2f               %.3f\n", i, drag);
    }
  }
  /* Print the footer for the chart. */
  printf("-------------------------------\n");
}

/* Fetch the data from the user and pass it on. */
void fetch_data(void){
  double CD, /* Holds the drag coefficient. */
         A; /* Holds the area. */
  /* Ask the user to enter the drag coefficient. */
  printf("Enter drag coefficient: ");
  /* Then store it into 'CD'. */
  scanf(" %lf", &CD);
  /* If the coefficient is within the streamline range. */
  if(CD >= STREAMLINE_MIN && CD <= STREAMLINE_MAX){
    /* Ask the user to enter the area. */
    printf("Enter area: ");
    /* Then store it into 'A'. */
    scanf(" %lf", &A);
    /* Calls the 'print_drag' function. */
    print_drag(CD,A);
  /* If the coefficient is not within the streamline range. */
  }else{
    /* Print an error asking the user to enter another number. */
    printf("Error: Please enter a number within the streamlined range (0.2 ... 0.5).\n");
    /* Recall 'fetch_data'. */
    fetch_data();
  }
}

/* The main function of the program. Calls the 'fetch_data' function to start. */
int main(void){
  /* Calls the 'fetch_data' function. */
  fetch_data();
  /* Return zero to satisfy the interger return of the main function. */
  return 0;
}