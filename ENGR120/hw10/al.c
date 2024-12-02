/*
  Programmer: Andrew Januszko     Date of Creation: 20 April 2019
  Instructor: Chen Huo            Course: ENGR 120 - 03

  A external C file that contains all the functions required for "main_al.c"
*/
#include <stdio.h> /* printf() definition. */
#include <stdlib.h> /* malloc(), memmove(), free(), definitions. */
#include <string.h>
#include "array_list.h" /* Function definition. */

/* Make a new array_list from the provided string. */
array_list_t *make_list(char letters[], int n) {
  array_list_t *lst = (array_list_t *)malloc(sizeof(array_list_t));
  lst -> size = n;
  for(int i = 0; i < size(lst); i++){
    lst -> arr[i] = letters[i];
  }
  return lst;
}

/* Add a character to the end of the array_list. */
void append(array_list_t *lst, char c) {
  lst -> size += 1;
  memmove(lst -> arr + (size(lst) - 1) + 1, lst -> arr + (size(lst) - 1), size(lst) - ((size(lst) - 1) - 1));
  lst -> arr[(lst -> size - 1)] = c;
}

/* Add a character to the start of the array_list. */
void prepend(array_list_t *lst, char c) {
  lst -> size += 1;
  memmove(lst -> arr + 0 + 1, lst -> arr + 0, size(lst) - (0 - 1));
  lst -> arr[0] = c;
}

/* Insert a character to the correct position in the array_list. */
void insert_at(array_list_t *lst, char c, int pos) {
  if(!pos) {
    prepend(lst, c);
  }else{
    lst -> size += 1;
    memmove(lst -> arr + pos + 1, lst -> arr + pos, size(lst) - (pos - 1));
    lst -> arr[pos] = c;
  }
}

/* Fetch the size of the array_list. */
int size(array_list_t *lst) {
  return lst -> size;
}

/* Remove a character at the provided position in the array_list. */
void remove_ith(array_list_t *lst, int pos) {
  lst -> size -= 1;
  memmove(&lst -> arr[pos], &lst -> arr[pos + 1], size(lst) - pos);
}

/* Delete the array_list. */
void delete_list(array_list_t *lst) {
  free(lst);
}

/* Print the array_list. */
void print_list(array_list_t *lst) {
  printf("[");
  for(int i = 0; i < size(lst); i++){
    if(i == (size(lst) - 1)){
      printf(" %c ]\n", lst -> arr[i]);
      break;
    }
    printf(" %c |", lst -> arr[i]);
  }
}
