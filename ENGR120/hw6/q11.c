/*
  Programmer: Andrew Januszko         Date of Creation: 03/24/19
  Instructor: Chen Huo                Course: ENGR 120 - 03

  A program that merges two arrays and sorts the in acending order.
*/
#include <stdio.h> /* printf, scanf defintions. */
#define MAX_LENGTH 10 /* Holds the max length of a line. */
#define INPUT "q11_input.txt" /* Holds the title of the input file. */

/* Sorts the values of the array in ascending order. */
void bubblesort(int *sorted, int length) {
  /* Sorts the array by comparing values in the array. */
  for (int i = 0; i < length; i++) {
    for (int j = 0; j < length - i - 1; j++) {
      if (sorted[j] > sorted[j + 1]) {
        int temp = sorted[j];
        sorted[j] = sorted[j + 1];
        sorted[j + 1] = temp;
      }
    }
  }
}

/* Fetches the two lines from the file. */
void fetch_arrays(FILE *input) {
  /* Holds the maximum length of the large array. */
  int length = MAX_LENGTH * 2;
  int x[MAX_LENGTH], /* Holds the first array. */
      y[MAX_LENGTH], /* Holds the second array. */
      sorted[length]; /* Holds the sorted array. */
  /* While not reading in the end of file. */
  do{
    /* Fetch the arrays from the file. */
    for(int i = 0; i < MAX_LENGTH; i++) {
      fscanf(input, "%d", &x[i]);
    }
    if (feof(input)) {
      break;
    }
    for(int i = 0; i < MAX_LENGTH; i++) {
      fscanf(input, "%d", &y[i]);
    }
    if (feof(input)) {
      break;
    }
  }while(!feof(input));
  /* Pass the first array into the large array. */
  for(int i = 0; i < MAX_LENGTH; i++) {
    sorted[i] = x[i];
  }
  /* Pass the second array into the large array. */
  for(int i = MAX_LENGTH; i < length; i++) {
    sorted[i] = y[i - MAX_LENGTH];
  }
  /* Calls the 'bubble_sort' function. */
  bubblesort(sorted, length);
  /* Prints the sorted array. */
  for(int i = 0; i < length; i++) {
    printf("%d ", sorted[i]);
  }
  printf("\n");
}

/* The driver function for the program. */
void driver(void) {
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
    return;
  }else {
    /* Calls the 'fetch_arrays' function. */
    fetch_arrays(input);
    /* Closes the input file. */
    fclose (input);
  }
}

int main(void) {
  /* Calls the 'driver' function. */
  driver();
  /* Return 0. */
  return 0;
}