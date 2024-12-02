/*
 Programmer: Andrew Januszko        Date of Creation: 02/20/19
 Instructor: Chen Huo               Course: ENGR 120 - 03
 
 A program the guesses the substance in a cylinder by the color.
*/
#include <stdio.h> /* printf, scanf definitions. */

/* Definition for the 'cylinder_color' function. */
void cylinder_color(void);


int main(void){
  /* Call the 'cynlinder_color' function. */
  cylinder_color();
    
  /* Return 0. */
  return(0);
}

/* Guess the substance from the first letter of the color. */
void cylinder_color(void){
  char gasColor; /* Holds the first letter of the color. */
    
  /* Ask the user for the first letter of the color. */
  printf("Enter the first letter of the cylinder's color: ");

  /* Store the letter in the gasColor variable. */
  scanf("%c", &gasColor);
    
  /* Switch statement for color. */
  switch (gasColor) {
    /* If 'O' or 'o'. */
    case 'O':
    case 'o':
      /* Print that the substance is ammonia. */
      printf("\nThis cylinder contains ammonia.\n");
      break;
    /* If 'B' or 'b'. */
    case 'B':
    case 'b':
      /* Print that the substance is carbon monoxide. */
      printf("\nThis cylinder contains carbon monoxide.\n");
      break;
    /* If 'Y' or 'y'. */
    case 'Y':
    case 'y':
      /* Print that the substance is hydrogen. */
      printf("\nThis cylinder contains hydrogen.\n");
      break;
    /* If 'G' or 'g'. */
    case 'G':
    case 'g':
      /* Print that the substance is oxygen. */
      printf("\nThis cylinder contains oxygen.\n");
      break;
    /* If else. */
    default:
      /* Print that the substance is unknown. */
      printf("\nContents unknow.\n");
      break;
  }
}
