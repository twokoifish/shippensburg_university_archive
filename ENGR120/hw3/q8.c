
/*
 Programmer: Andrew Januszko        Date of Creation: 02/22/19
 Instructor: Chen Huo               Course: ENGR 120 - 03
 
 Test to see if you emitter too many pollutants.
*/
#include <stdio.h> /* printf, scanf definitions. */

#define CM_FIRST 3.4 /* Carbon monoxide under 50k emission max. */
#define CM_SECOND 4.2 /* Carbon monoxide over 50k emission max. */
#define HC_FIRST 0.31 /* Hydrocarbons under 50k emission max. */
#define HC_SECOND 0.39 /* Hydrocarbons over 50k emission max. */
#define NO_FIRST 0.4 /* Nitrogen oxides under 50k emission max. */
#define NO_SECOND 0.5 /* Nitrogen oxides over 50k emission max. */
#define NH_FIRST 0.25 /* Nonmethane hydrocarbons under 50k emission max. */
#define NH_SECOND 0.31 /* Nonmethane hydrocarbons over 50k emission max. */

/* Definition for the 'pollution' function. */
void pollution(int pollutantNumber,
               double gramsPerMile,
               double odometerReading,
               double limits[4][2]);

int main(void){
  /* Holds the pollutans first 50k and next 50k max values. */
  double limits[][2] = {
                        {CM_FIRST, CM_SECOND},
                        {HC_FIRST, HC_SECOND},
                        {NO_FIRST, NO_SECOND},
                        {NH_FIRST, NH_SECOND}
                       };
    
  int pollutantNumber; /* Holds the chosen pollutant number. */
  double gramsPerMile, /* Holds the emitted grams per mile. */
         odometerReading; /* The odometer reading of the vehicle. */

  /* Ask the user to pick a pollutant number, then store it. */
  printf("(1) Carbon monoxide\n(2) Hydrocarbons\n(3) Nitorgen oxides\n(4) Nonmethane hydrocarbons\n");
  printf("Enter pollutant number>> ");
  scanf("%d", &pollutantNumber);
    
  /* Ask the user how many grams per mile they emitted of the pollutant. */
  printf("Enter number of grams emitted per mile>> ");
  scanf("%lf", &gramsPerMile);
    
  /* Ask the user for their vehicles odometer reading, then store it. */
  printf("Enter odometer reading>> ");
  scanf("%lf", &odometerReading);

  /* Call the 'pollution' function, and pass in the variables. */
  pollution(pollutantNumber, gramsPerMile, odometerReading, limits);
    
  /* Return 0. */
  return 0;
}

/* With the passed in varibles, see it they emitted too many pollutants given their odometer rating. */
void pollution(int pollutantNumber, double gramsPerMile, double odometerReading, double limits[][2]){
  /* Check to see if the chose a listed pollutant. */
  if(pollutantNumber >= 1 && pollutantNumber <= 4){
    /* Call the firstFifty and seconFifty value from the limits array for the pollutant. */
    double firstFifty = limits[pollutantNumber - 1][0];
    double secondFifty = limits[pollutantNumber - 1][1];
      
    /* If the odometer reading is less than 50k. */
    if(odometerReading <= 50000){
      /* If they exceed the limit. */
      if(gramsPerMile >= firstFifty) {
        /* Notify them of their emission rate. */
        printf("Emissions exceed permitted level of %.2f grams/mile\n", firstFifty);
      /* If they do not exceed the limit. */
      }else{
        /* Notify them of their emission rate. */
        printf("Emissions did not exceed permitted level of %.2f grams/mile\n", firstFifty);
      }
    /* If the odometer reading is greater than 50k. */
    } else {
      /* If they exceed the limit. */
      if(gramsPerMile >= secondFifty){
        /* Notify them of their emission rate. */
        printf("Emissions exceed permitted level of %.2f grams/mile\n", secondFifty);
      /* If they do not exceed the limit. */
      }else{
        /* Notify them of their emission rate. */
        printf("Emissions did not exceed permitted level of %.2f grams/mile\n", secondFifty);
      }
    }
  /* They did not enter an actual pollutant. */
  }else{
    printf("No such pollutant!\n");
  }
}
