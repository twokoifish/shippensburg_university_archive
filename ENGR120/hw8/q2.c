/*
  Programmer: Andrew Januszko         Date: 04/11/19
  Instructor: Chen Huo                Course: ENGR 120 - 03

  A program that prints the information of an element provided
  by the user.
*/
#include <stdio.h> /* printf() definitions. */
#include <string.h> /* strtok() definitions. */
#define MAX_INPUT 1024 /* Holds the max size of input. */

/* Holds the data for the element. */
typedef struct {
  int atomic_number;
  char name[MAX_INPUT];
  char class[MAX_INPUT];
  double atomic_weight;
  int electron_shell[7];
} element_t;

/* Scans the data to be stored for the element. */
void scan_element(element_t *element, char buffer[MAX_INPUT]);
/* Prints the data about the element. */
void print_element(element_t element);

int main(void) {
  /* Holds the element data. */
  element_t element;
  /* Holds the max size of input. */
  char buffer[MAX_INPUT];
  /* Calls scan element and returns the data of the element. */
  scan_element(&element, buffer);
  /* Print a new line. */
  printf("\n");
  /* Print the element data. */
  print_element(element);
  /* Return 0. */
  return 0;
}

/* Scans the data to be stored for the element. */
void scan_element(element_t *element, char buffer[MAX_INPUT]) {
  /* Get the atomic number and store it. */
  printf("Enter atomic #: ");
  fgets(buffer, MAX_INPUT, stdin);
  sscanf(buffer, "%d", &(*element).atomic_number);
  /* Get the element name and store it. */
  printf("Enter name: ");
  fgets(buffer, MAX_INPUT, stdin);
  sscanf(buffer, "%[^\n]", (*element).name);
  /* Get the element class and store it. */
  printf("Class: ");
  fgets(buffer, MAX_INPUT, stdin);
  sscanf(buffer, "%[^\n]", (*element).class);
  /* Get the atomic weight and store it. */
  printf("Weight: ");
  fgets(buffer, MAX_INPUT, stdin);
  sscanf(buffer, "%lf", &(*element).atomic_weight);
  /* Get the electron shells and store them. */
  printf("Enter electron shell: ");
  fgets(buffer, MAX_INPUT, stdin);
  int i = 0;
  char *token = strtok(buffer, " ");
  while((token != NULL) && (i < 7)) {
    sscanf(token, "%d", &(*element).electron_shell[i++]);
    token = strtok(NULL, " ");
  }
}

/* Prints the data about the element. */
void print_element(element_t element) {
  /* Print the atomic number and name. */
  printf("%d  - %s\n", element.atomic_number, element.name);
  /* Print the atomic weight. */
  printf("Weight: %.1f\n", element.atomic_weight);
  /* Print the element class. */
  printf("Class: %s\n", element.class);
  /* Print the electron shells. */
  printf("Shells:");
  for (int i = 0; i < 7; i++) {
    printf(" %d", element.electron_shell[i]);
  }
  /* Print a new line. */
  printf("\n");
}