/*
  Programmer: Andrew Januszko         Date of Creation:  03/29/2019
  Instructor: Chen Huo                Course: ENGR 120 - 03

  A program that calculates the resistance of a resistor based on the color 
  of the bands on its exterior.
*/
#include <stdio.h> /* printf(), scanf() definitions. */
#include <math.h> /* pow() definition. */
#include <string.h> /* strcpy(), strcmp() definitions. */
#define MAX_COLORS 10 /* The maximum number of colors on the resistor. */
#define BASE 10 /* The base value of the power. */
#define MAX_LENGTH 7 /* The max length of the colors. */
#define OHM_TO_KILOOHM (1.0/1000.0) /* Converts ohms to kilo - ohms. */
#define BANDS 3 /* The number of bands on the resistor. */

/* Walks the colors array and checks to see if the user input is valid. */
int search(char colors[MAX_COLORS][MAX_LENGTH], 
           int list_size,
           char input[MAX_COLORS]);
/* Calculates the resistance of the resistor. */
double calc_resistor(int bands[BANDS]);

int main(void) {
  /* Holds the user response to the ending prompt. Default value is "y". */
  char userResponse = 'y';
  /* Run while the user response is equal to "y". */
  do{
    /* Holds the invalid strings that the user may enter. */
    char errors[BANDS][MAX_LENGTH];
    int remaining_colors = BANDS, /* Holds the remaining bands. */
        band = 1, /* Holds the current band number. */
        invalid_colors = 0, /* Holds the number of invalid bands. */
        bands[BANDS]; /* Holds the band #s from search(). */
    /* Prints the starting prompt. */
    printf("Enter the colors of the resistor's three bands, beginning with\n"
         "the band nearest the end. Type the colors in lowercase letters"
         " only, NO CAPS.\n");
    /* While there are still bands to scan in. */
    while(remaining_colors) {
      /* Holds the colors to check. */
      char COLOR_CODES[MAX_COLORS][MAX_LENGTH] = {"black", "brown", "red",
        "orange", "yellow", "green", "blue", "violet", "gray", "white"};
      /* Holds the user input. */
      char input[MAX_COLORS];
      /* Print the band # to enter, and store user input. */
      printf("Band %d => ", band);
      scanf(" %s", input);
      /* Store the returned value of search() into the band array. */
      bands[band - 1] = search(COLOR_CODES, MAX_COLORS, input);
      /* If the returned value is -1, mark the user input as invalid. */
      if(bands[band - 1] == -1) {
          /* Add the invalid color to the errors array. */
          strcpy(errors[invalid_colors], input);
          /* Increment. */
          ++invalid_colors;
      } 
      /* Increment. */
      ++band;
      /* Reduce. */
      --remaining_colors;
    }
    /* If there are invalid colors. */
    if(invalid_colors) {
      /* Print out their names. */
      for(int i = 0; i < invalid_colors; i++) {
        printf("Invalid color: %s\n", errors[i]);
      }
    /* Otherwise, get the resistance. */
    }else{
      /* Print the resistance of the resistor. */
      printf("Resistance value: %.1f kilo-ohms\n", calc_resistor(bands));
    }
    /* Ask the user if they would like to decode another resistor. */
    printf("Do you want to decode another resistor?\n"
           "=>");
    scanf(" %c", &userResponse);    
  }while(userResponse == 'y');
  /* Return 0. */
  return 0;
}

/* Walks the colors array and checks to see if the user input is valid. */
int search(char colors[MAX_COLORS][MAX_LENGTH], 
           int list_size,
           char input[]) {
  for(int i = 0; i < list_size; ++i) {
    /* If the user input is valid, return the # of the band color. */
    if(strcmp(input, colors[i]) == 0) {
      return i;
    }
  }
  /* Otherwise, return -1. */
  return -1;
}

/* Calculates the resistance of the resistor. */
double calc_resistor(int bands[BANDS]) {
  /* Calculate the resistance in ohms. */
  double ohms = ((bands[0] * 10) + bands[1]) * pow(BASE, bands[2]);
  /* Return the resistance in kilo - ohms. */
  return ohms * OHM_TO_KILOOHM;
}