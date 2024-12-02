/*
 Programmer: Andrew Januszko        Date of Creation: 03/19/19
 Instructor: Chen Huo               Course: ENGR 120 - 03

 A program that takes in an input file, calculates the area of the
 stored polygon using its points. Then outputs the points up to
 six decimal points.
*/
#include <stdio.h> /* printf, scanf definitions. */
#include <math.h> /* fabs definitions. */
#define MAX_POINTS 20  /* maximum amount of points in a file. */
#define INPUT "q2_input.txt" /* title of the input file. */
#define OUTPUT "q2_output.txt" /* title of the output file. */

/* Calculates the area of the polygon. */
void polygon_area(double x[MAX_POINTS], double y[MAX_POINTS], double *A) {
  int n = 0, /* Holds the number of points in the polygon. */
      match = 0;  /* Holds the bianary value for true/false in the loop. */
  double sigma = 0; /* Holds the loops value. */
  /* Loop that calculates the points in the file. */
  for(int i = 1; i < MAX_POINTS && !match; i++) {
    if(x[0] == x[i] && y[0] == y[i]) {
      match = 1;
      n = i + 1;
    }
  }
  /* Loop that calculates the sigma value using the points. */
  for(int i = 0; i < n - 2; i++) {
    sigma += ((x[i + 1] + x[i]) * (y[i + 1] - y[i]));
  }
  /* Calculate the area of the polygon. */
  *A = (0.5) * fabs(sigma);
}

/* Store the corners of the polygon into the output file. */
void output_corners(FILE *output, double x[MAX_POINTS], double y[MAX_POINTS]) {
  int n = 0, /* Holds the number of points in the polygon. */
      match = 0;  /* Holds the bianary value for true/false in the loop. */
  /* Loop that calculates the points in the file. */
  for(int i = 1; i < MAX_POINTS && !match; i++) {
    if(x[0] == x[i] && y[0] == y[i]) {
      match = 1;
      n = i + 1;
    }
  }
  /* Writes he coordinates to the file. */
  for(int i = 0; i < n; i++) {
    if(i < n - 1) {
      fprintf(output, "%lf %lf\n", x[i], y[i]);
    }else{
      fprintf(output, "%lf %lf", x[i], y[i]);
    }
  }
}

/* Fetch the corners of the polygon from the input file. */
void get_corners(FILE *input, double *x, double *y) {
  double inX, /* Holds the temp x value of the file. */
         inY; /* Holds the temp y value of the file. */
  int match = 0;  /* Holds the bianary value for true/false in the loop. */
  /* Scans in the numbers while the line has 2 values.
  
     Walks through the file line by line until the ending
     value is the same as the starting value.
   */
  while(fscanf(input, "%lf %lf", &inX, &inY) == 2) {
    x[0] = inX;
    y[0] = inY;
    for(int i = 1; i < MAX_POINTS && !match; i++) {
      fscanf(input, "%lf %lf", &inX, &inY);
      x[i] = inX; y[i] = inY;
      if(x[0] == inX && y[0] == inY) {
         match = 1;
      }
    }
  }
}

/* The driver function for the program. */
void driver(void) {
  /* Holds the input file for the program. */
  FILE *input = fopen(INPUT, "r");
  /* Holds the output file for the program. */
  FILE *output = fopen(OUTPUT, "w");
  double x[MAX_POINTS], /* Holds the x coordinates. */
         y[MAX_POINTS], /* Holds the y coordinates. */
         A; /* Holds the area of the polygon. */
  int points = 0, /* Holds the number of points in the file. */
      match = 0; /* Holds the bianary value for true/false in the loop. */
  /* If the input file does not exist. */
  if(input == NULL) {
    /* Print that the file does not exist. */
    fprintf(stderr, "\"%s\" does not exist or the file cannot be opened.\n"
                    "Please verify that the file is in the "
                    "same directory as this program.\n", INPUT);
    /* Close the input and output files. */
    fclose (input); fclose (output);
    /* Exit the function. */
    return;
  }else{
    /* Calls the 'get_corners' function. */
    get_corners(input, x, y);
    /* Calls the 'output_corners' function. */
    output_corners(output, x, y);
    /* Calls the 'polygon_area' function. */
    polygon_area(x, y, &A);
    /* Loop that calculates the points in the file. */
    for(int i = 1; i < MAX_POINTS && !match; i++) {
      if(x[0] == x[i] && y[0] == y[i]) {
        match = 1;
        points = i + 1;
      }
    }
    /* Print the points and the area of the file. */
    printf("The area of %d points is %.6f.\n", points, A);
    /* Close the input and output files. */
    fclose (input); fclose (output);
  }
}

int main(void) {
  /* Calls the 'driver' function. */
  driver();
  /* Return 0. */
  return 0;
}