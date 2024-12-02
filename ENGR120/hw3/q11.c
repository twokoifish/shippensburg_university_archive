/*
 Programmer: Andrew Januszko        Date of Creation 02/24/19
 Instructor: Chen Huo               Course: ENGR 120 - 03
 
 See what boiling point the user is trying to determine.
*/
#include <stdio.h> /* printf, scanf definitions. */

#define BP_W 100.0 /* Boiling point of water. */
#define BP_M 357.0 /* Boiling point of mercury. */
#define BP_C 1187.0 /* Boiling point of copper. */
#define BP_S 2193.0 /* Boiling point of silver. */
#define BP_G 2660.0 /* Boiling point of gold. */
#define TO_PERCENT (1.0/100.0) /* Convert a number to a percentage. */

/* Definition for 'within_x_percent' function. */
int within_x_percent(double ref, double data, double x);

int main(void){
  int i = 0; /* Holds the index for our boiling points. */
  double data, /* Holds the user definined boiling point. */
         x; /* Holds the percentage range for the boiling point. */
  /* Holds the boiling points as an array so we can walk it. */
  double boiling_point[5] = {BP_W, BP_M, BP_C, BP_S, BP_G};

  /* Holds the boiling points by name so we can walk and print the correct one. */
  char *substance[5] = {"Water",
                        "Mercury",
                        "Copper",
                        "Silver",
                        "Gold"};

  /* Ask the user for a boiling point, then store it. */
  printf("Enter boiling point: ");
  scanf("%lf", &data);
    
  /* Ask the user for a range, then store it. */
  printf("Enter range in percentage: ");
  scanf("%lf", &x);
    
  /* While less than the length of the array. */
  while(i < 5){
    /* If the boiling point fits in the range. */
    if(within_x_percent(boiling_point[i], data, x) == 1){
      /* Print the substance name. */
      printf("%s\n", substance[i]);
      break;
    /* If the boiling point does not fit in the range. */
    }else{
      /* Increment the array. */
      i = i + 1;
    }
  }
  /* If the boiling point is not in the array. */
  while(i == 5){
    /* Print that the substance is unknown. */
    printf("Substance unknown\n");
    break;
  }
  /* Return 0. */
  return 0;
}

/* Determines if the boiling point is in the range of the defined ones. */
int within_x_percent(double ref, double data, double x){
  /* Convert x to a percentage. */
  x = x * TO_PERCENT;
  /* If it does fall in the range. */
  if ((data >= ref - x * ref) && (data <= ref + x * ref)) {
    /* Return 1. */
    return 1;
  /* If it does not fall in the range. */
  }else{
    /* Return 0. */
    return 0;
  }
}
