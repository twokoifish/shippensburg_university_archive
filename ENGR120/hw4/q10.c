/*
 Programmer: Andrew Januszko    Date of Creation: 03/03/19
 Instructor: Chen Huo           Course: ENGR 120 - 03

 Calculate pressure at a given volume.
*/
#include <stdio.h> /* printf, scanf definitions. */
#include <math.h> /* pow() definition. */

#define a 3.592 /* Constant. */
#define b 0.0427 /* Constant. */
#define R 0.08206 /* Constant. */

/* Definition for 'vanDerWaal' function. */
void vanDerWaal(double n, double T, double Vi, double Vf, double i);

int main(void){
  double n, /* Holds the moles. */
         T, /* Holds the temperature. */
         Vi, /* Holds the initial volume. */
         Vf, /* Holds the final volume. */
         i; /* Holds the number to increment by. */
  
  /* Ask the user for the numebr of moles, then store it. */
  printf("Quantity of carbon dioxide (moles)> ");
  scanf("%lf", &n);

  /* Ask the user for the temperature in kelvin, then store it. */
  printf("Temperature (kelvin)> ");
  scanf("%lf", &T);

  /* Ask the user for the initial volume, then store it. */
  printf("Initial volume (milliliters)> ");
  scanf("%lf", &Vi);

  /* Ask the user for the final volume, then store it. */
  printf("Final volume (milliliters)> ");
  scanf("%lf", &Vf);

  /* Ask the user for the increment rate, then store it. */
  printf("Volume increment (milliliters)> ");
  scanf("%lf", &i);

  /* Print new line. */
  printf("\n");

  /* Call function and pass 'n, T, Vi, Vf, i'. */
  vanDerWaal(n, T, Vi, Vf, i);

  /* Return 0. */
  return 0;
}

/* Calculate the pressure at the given volume. */
void vanDerWaal(double n, double T, double Vi, double Vf, double i){
  double volume, /* Holds the volume we increment. */
         P; /* Holds the pressure . */

  /* Convert the initial volume to liters. */
  volume = Vi/1000;

  /* Convert the final volume to liters. */
  Vf = Vf/1000;

  /* Convert the increment to liters. */
  i = i/1000;

  /* Print the moles and the temperature. */
  printf("%.6f moles of carbon dioxide at %.1f K\n", n, T);
  /* Print the volume and pressure. */
  printf("Volume (l)\t\tPressure (atm)\n\n");
  /* While volume is less than the largest volume. */
  do{
    /* Calculate the pressure. */
    P = ((n*R*T)/(volume-(b*n))) - ((a*pow(n,2))/pow(volume,2));
    /* Print the volume and the pressure. */
    printf("%.6f\t\t\t\t%.4f\n", volume, P);
    /* Increment the volume. */
    volume += i;
  }while(volume < (Vf + i));
}
