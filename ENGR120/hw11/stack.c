#include "stack.h"
#include <stdlib.h>
#include <stdio.h>

/* Initialize an empty stack */
engr120_stack_t init() {
  engr120_stack_t stack = NULL;
  return (stack);
}

/* Print the whole stack */
void show(engr120_stack_t s) {
  if(s == NULL){
    printf("NULL\n");
  }
  printf("%d -> ", (*s).number);
  show(s -> next);
}

/* Pop the top element off the stack and return the element */
int pop(engr120_stack_t *s) {
  int val = (*s) -> number;
  *s = remove_ith(val, 0);
  return val;
}

/* Push char c to the stack */
void push(engr120_stack_t *s, int c) {
  *s = prepend(*s, c);
} 

/* Return true if the stack is empty */
int is_empty(engr120_stack_t s) {
  return (s == NULL);
}
