/*
  Programmer: Andrew Januszko         Date of Creation: 03/24/19
  Instructor: Chen Huo                Course: ENGR 120 - 03

  A program that states the validity of a UPC code.
*/
#include <stdio.h> /* printf, scanf definitions. */
#define CODE_LENGTH 12 /* The maximum length of the UPC code. */
#define VALID 1 /* Holds the constant for a valid code. */
#define INVALID 0 /* Holds the constant for an invalid code. */
#define INPUT "q5_input.txt" /* The name of the input file. */

/* Prints the validity of the UPC code and its check digit. */
void print_validity(int *UPC, int check_digit, int validity) {
  /* While i is less than the length of the UPC code. */
  for(int i = 0; i < CODE_LENGTH; i++) {
    /* Print the UPC code. */
    printf("%d ", UPC[i]);
  }
  /* Check the validity of the UPC code. */
  switch (validity) {
    case 1:
      printf("valid\n");
      break;
  
    default:
      printf("digit %d does not match %d\n", check_digit, UPC[11]);
      break;
  }
}

/* Calculates the check digit from the UPC codes. */
void check_digit(int *UPC) {
  int odd = 0, /* Holds the odd value of the code. */
      even = 0, /* Holds the even value of the code. */
      sum = 0, /* Holds the sum of the code. */
      check_digit = 0; /* Holds the check digit of the code. */
  /* Calculate the even value for the UPC code. */
  for(int i = 1; i <= 9; i += 2) {
    even += UPC[i];
  }
  /* Calculate the odd value for the UPC code. */
  for(int i = 0; i <= 10; i += 2) {
    odd += UPC[i];
  }
  odd = odd * 3;
  /* Calculate the sum. */
  sum = odd + even;
  /* Calculate the check digit. */
  check_digit = sum % 10;
  /* If the check digit is equal to zero, check validity. */
  if(check_digit == 0) {
    if(UPC[11] == 0) {
      print_validity(UPC, 0, VALID);
    }else {
      print_validity(UPC, 0, INVALID);
    }
  /* If the check digit is not equal to zero, check validity. */
  }else {
    check_digit = 10 - check_digit;
    if(UPC[11] == check_digit) {
      print_validity(UPC, check_digit, VALID);
    }else {
      print_validity(UPC, check_digit, INVALID);
    }
  }
}

/* Fetches the UPC codes from the file. */
void fetch_UPC(FILE *input, int *UPC) {
  /* Do while there is a line to scan in.
     Break when you reach EOF. */
  do{
    for(int i = 0; i < CODE_LENGTH; i++) {
      fscanf(input, "%d", &UPC[i]);
    }
    if (feof(input)) {
      break;
    }
    /* Calls the 'check_digit' function. */
    check_digit(UPC);
  }while(!feof(input));
}

/* The driver function for the program. */
void driver(void) {
  /* Holds the input file for the program. */
  FILE *input = fopen(INPUT, "r");
  /* Holds the UPC codes. */
  int UPC[CODE_LENGTH];
  /* If the input file does not exist. */
  if(input == NULL) {
    /* Print that the file does not exist. */
    fprintf(stderr, "\"%s\" does not exist or the file cannot be opened.\n"
                    "Please verify that the file is in the "
                    "same directory as this program.\n", INPUT);
    /* Close the input and output files. */
    fclose (input);
    /* Exit the program. */
    return;
  }else {
    /* Calls the 'fetch_UPC' function. */
    fetch_UPC(input, UPC);
    /* Close the input and output files. */
    fclose(input);
  }
}

int main(void) {
  /* Calls the 'driver' function. */
  driver();
  /* Return 0. */
  return 0;
}