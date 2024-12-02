#include "stack.h"
#include <stdlib.h>
#include <stdio.h>

engr120_stack_t init() {
  engr120_stack_t stack = NULL;
  return stack;
}

void show(engr120_stack_t s) {
  if(s == NULL){
    printf("NULL\n");
  }
  printf("%d -> ", (*s).number);
  show(s -> next);
}

/* dequeue */
int pop(engr120_stack_t *s) {
  
}

/* insert_At(lst, 0) */
void push(engr120_stack_t *s, int i) {
  
}

int is_empty(engr120_stack_t q) {
  return (q == NULL);
}
