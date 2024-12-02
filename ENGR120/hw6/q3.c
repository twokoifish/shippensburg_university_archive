/*
  Programmer: Andrew Januszko         Date of Creation: 03/24/19
  Instructor: Chen Huo                Course: ENGR 120 - 03

  Program that calculates the point mass from an input file.
*/
#include <stdio.h> /* printf, scanf definitions. */
#define MAX_N 10 /* maximum possible value for n. */
#define INPUT "q3_input.txt" /* title of the input file. */

/* Prints the point mass, locations, masses, and n from the file. */
void fwrite_point_mass(int location[][3], int *mass, int n){
  /* Print the location and the points for it. */
  printf("Location:\n");
  for(int i = 0; i < n; i++){
    for(int j = 0; j < n - 1; j++){
      if(j == n - 2){
        printf("%d.000000", location[i][j]);
      }else{
        printf("%d.000000 ", location[i][j]);
      }
    }
    printf("\n");
  }
  /* Print the masses and the values for them. */
  printf("Mass:\n");
  for(int i = 0; i < n; i++){
      printf("%d.000000\n", mass[i]);
  }
  /* Print the value of n. */
  printf("n: %d\n", n);
}

/* Calculates the center of gravity from the points. */
double center_grav(int location[][3], int *mass, int n) {
  /* Print the center of mass and the n values from it. */
  printf("======================\n");
  printf("Center of mass: (");
  int netMass = 0; /* Holds the net mass of the masses. */
  double maths; /* Holds the math done by the function. */
  /* Calculate the net mass. */
  for(int i = 0; i < n; i++){
    netMass += mass[i];
  }
  /* Calculate the value of maths. */
  for(int j = 0; j < n - 1; j++){
    maths = 0;
    for(int i = 0; i < n; i++){
      int m = mass[i];
      int l = location[i][j];
      maths += (m * l);
    }
    maths = ((1.0/netMass) * maths);
    /* Print the center of point mass. */
    if(j == n - 2){
      printf("%f", maths);
    }else{
      printf("%f ", maths);
    }
  }
  /* Stop printing the center mass. */
  printf(")\n");
  /* Return the value of maths. */
  return maths;
}

/* Fetchs the points and masses needed to calculate the point mass. */
int fget_point_mass(FILE *input, int n, int location[][3], int *mass) {
  /* Scan in the n value for the file. */
  fscanf(input, "%d", &n);
  /* Take in points and masses until the end of the file is reached. */
  do{
    for(int i = 0; i < n; i++){
      for(int j = 0; j < n - 1; j++){
        fscanf(input, " %d", &location[i][j]);
      }
      fscanf(input, " %d", &mass[i]);
    }
    if (feof(input)) {
      break;
    }
  }while(!feof(input));
  /* Return the value of n. */
  return n;
}

int main(void) {
  /* Holds the 3D points from the file. */
  int location[MAX_N][3];
  /* Holds the masses from the file. */
  int mass[MAX_N];
  /* Holds the number of points from the file. */
  int n = 0;
  /* Holds the input file for the program. */
  FILE *input = fopen(INPUT, "r");
  /* If the input file does not exist. */
  if(input == NULL) {
    /* Print that the file does not exist. */
    fprintf(stderr, "\"%s\" does not exist or the file cannot be opened.\n"
                    "Please verify that the file is in the "
                    "same directory as this program.\n", INPUT);
    /* Close the input and output files. */
    fclose (input);
    /* Exit the program. */
    return 1;
  }else {
    /* n is equal to the function return value. */
    n = fget_point_mass(input, MAX_N, location, mass);
    /* Calls the 'center_grav' function. */
    center_grav(location, mass, n);
     /* Calls the 'fwrite_point_mass' function. */
    fwrite_point_mass(location, mass, n);
    /* Close the input and output files. */
    fclose (input);
  }
  /* Return 0. */
  return 0;
}