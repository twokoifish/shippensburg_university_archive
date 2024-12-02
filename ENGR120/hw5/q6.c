/*
 Programmer: Andrew Januszko      Date of Creation: 03/10/19
 Instructor: Chen Huo             Course: ENGR 120 - 03

 A collection of functions that can be used to solve simple conduction problems.
*/
#include <stdio.h> /* printf(), fgets() definitions. */
#include <stdlib.h> /* atof() definition. */

/* Calculates the 'H' based on the other user input. */
void calc_h(double *dH,
            double dk,
            double dA,
            double dT1, 
            double dT2, 
            double dX){
  /* Calculate 'dH' with the provided data. */
  *dH = -(((dk * dA) * (dT2 - dT1)) / dX);
}

/* Calculates the 'k' based on the other user input. */
void calc_k(double dH, 
            double *dk, 
            double dA, 
            double dT1, 
            double dT2, 
            double dX){
  /* Calculate 'dk' with the provided data. */
  *dk = -((dX * dH) / (dA * (dT2 - dT1)));
}

/* Calculates the 'A' based on the other user input. */
void calc_a(double dH, 
            double dk, 
            double *dA, 
            double dT1, 
            double dT2, 
            double dX){
  /* Calculate 'dA' with the provided data. */
  *dA = -((dX * dH) / (dk * (dT2 - dT1)));
}

/* Calculates the 'T1' based on the other user input. */
void calc_t1(double dH, 
             double dk, 
             double dA, 
             double *dT1, 
             double dT2, 
             double dX){
  /* Calculate 'dT1' with the provided data. */
  *dT1 = (((dX * dH) + (dk * dA * dT2)) / (dk * dA));
}

/* Calculates the 'T2' based on the other user input. */
void calc_t2(double dH, 
             double dk, 
             double dA, 
             double dT1, 
             double *dT2, 
             double dX){
  /* Calculate 'dT2' with the provided data. */
  *dT2 = -(((dX * dH) - (dk * dA * dT1)) / (dk * dA));
}

/* Calculates the 'X' based on the other user input. */
void calc_x(double dH, 
            double dk, 
            double dA, 
            double dT1, 
            double dT2, 
            double *dX){
  /* Calculate 'dX' with the provided data. */
  *dX = -(((dk * dA) * (dT2 - dT1)) / dH);
}

/* Prints the data, both calculated and supplied by the user. */
void print_data(double H, double k, double A, double T1, double T2, double X){
  /* Print the data from the 'calc_<>' function and the user input. */
  printf("H = %.1f W               T2 = %.0f K\n"
         "k = %.4f W/m-K               T1 = %.0f K\n"
         "A = %.3f m^2               X = %.4f K\n", H, T1, k, T2, A, X);
}

/* Fetch the data from 'get_data' and see what is missing. */
void fetch_missing(char *H, char *k, char *A, char *T1, char *T2, char *X){
  double dH = atof(H), /* Fetch the double value from 'H'. */
         dk = atof(k), /* Fetch the double value from 'k'. */ 
         dA = atof(A), /* Fetch the double value from 'A'. */
         dT1 = atof(T1), /* Fetch the double value from 'T1'. */ 
         dT2 = atof(T2), /* Fetch the double value from 'T2'. */ 
         dX = atof(X); /* Fetch the double value from 'X'. */
  /* If 'H' is equal to '?'. */
  if(*H == (int)'?'){
    /* Calls the 'calc_h' function and passes in every variable. */
    calc_h(&dH,dk,dA,dT1,dT2,dX);
    /* Prints the result from the function. */
    printf("Rate of head transfer is %.6f watts.\n", dH);
  /* If 'k' is equal to '?'. */
  }else if(*k == (int)'?'){
    /* Calls the 'calc_k' function and passes in every variable. */
    calc_k(dH,&dk,dA,dT1,dT2,dX);
    /* Prints the result from the function. */
    printf("Coefficient of thermal conductivity is %.6f W/m-K.\n", dk);
  /* If 'A' is equal to '?'. */
  }else if(*A == (int)'?'){
    /* Calls the 'calc_a' function and passes in every variable. */
    calc_a(dH,dk,&dA,dT1,dT2,dX);
    /* Prints the result from the function. */
    printf("Area of Conductor is %.6f m^2.\n", dA);
  /* If 'T1' is equal to '?'. */
  }else if(*T1 == (int)'?'){
    /* Calls the 'calc_t1' function and passes in every variable. */
    calc_t1(dH,dk,dA,&dT1,dT2,dX);
    /* Prints the result from the function. */
    printf("Temperature on one side is  %.0f K.\n", dT1);
  /* If 'T2' is equal to '?'. */   
  }else if(*T2 == (int)'?'){
    /* Calls the 'calc_t2' function and passes in every variable. */
    calc_t2(dH,dk,dA,dT1,&dT2,dX);
    /* Prints the result from the function. */
    printf("Temperature on other side is  %.0f K.\n", dT2);   
  /* If 'X' is equal to '?'. */
  }else if(*X == (int)'?'){
    /* Calls the 'calc_x' function and passes in every variable. */
    calc_x(dH,dk,dA,dT1,dT2,&dX);
    /* Prints the result from the function. */
    printf("Thickness %.4f m.\n", dX);
  }
  /* Calls the 'print_data' function and passes in every variable. */
  print_data(dH,dk,dA,dT1,dT2,dX);
}

/* Get data from the user and pass it into 'fetch_missing'. */
void get_data(){
  char H[11], /* Holds the data passed in from 'fgets()'. */
       k[11], /* Holds the data passed in from 'fgets()'. */
       A[11], /* Holds the data passed in from 'fgets()'. */
       T1[11], /* Holds the data passed in from 'fgets()'. */
       T2[11], /* Holds the data passed in from 'fgets()'. */
       X[11]; /* Holds the data passed in from 'fgets()'. */
  /* Tell the user to enter '?' is the value is unknown for the parameter. */
  printf("Respond to the prompts, enter ? for unknown:\n");
  /* Ask the user for the rate of heat transfer in watts. */
  printf("Rate of heat transfer (watts) >> ");
  /* Walk the line and store it into 'H'. */
  fgets(H, sizeof(H), stdin);
  /* Ask the user for the coefficient of thermal conductivity. */
  printf("Coefficient of thermal conductivity (W/m-K) >> ");
  /* Walk the line and store it into 'k'. */
  fgets(k, sizeof(k), stdin);
  /* Ask the user for the cross - sectional area of the conductor. */
  printf("Cross-sectional area of conductor: (W^2) >> ");
  /* Walk the line and store it into 'A'. */
  fgets(A, sizeof(A), stdin);
  /* Ask the user for the temperature on one side of the conductor. */
  printf("Temperature on one side (K) >> ");
  /* Walk the line and store it into 'T1'. */
  fgets(T1, sizeof(T1), stdin);
  /* Ask the user for the temperature on the other side of the conductor. */
  printf("Temperature on other side (K) >> ");
  /* Walk the line and store it into 'T2'. */
  fgets(T2, sizeof(T2), stdin);
  /* Ask the user for the thickness of the conductor. */
  printf("Thickness of conductor (m) >> ");
  /* Walk the line and store it into 'X'. */
  fgets(X, sizeof(X), stdin);
  /* Calls the 'fetch_missing' function and passes in all the variables. */
  fetch_missing(H,k,A,T1,T2,X);
}

/* The main function of the program. This calls the 'get_data' function to start the program. */
int main(void){
  /* Calls the 'get_data' function. */
  get_data();
  /* Return zero to satisfy the extected integer return value of the main function. */
  return 0;
}