#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "stack.h"

#define LEN 80

int calc(char op, int operand1, int operand2);

int
main(void)
{
  printf("Enter the postfix expression: ");
  char input[LEN];

  fgets(input, LEN, stdin);

  engr120_stack_t s = init();
  engr120_stack_t *stack = &s;

  char *token = strtok(input, " ");

  while (token != NULL) {
    show(*stack);
    int converted = atoi(token);

    if (converted || strcmp(token, "0") == 0) {
      push(stack, converted);
    } else {
      int operand1 = pop(stack);
      int operand2 = pop(stack);
      int result = calc(token[0], operand1, operand2);
      push(stack, result);
    }

    token = strtok(NULL, " ");
  }

  printf("Result is: %d\n", pop(stack));
}

int calc(char op, int operand1, int operand2) {
  switch (op) {
    case '+':
      return operand2 + operand1;
    case '-':
      return operand2 - operand1;
    case '/':
      return operand2 / operand1;
    case '*':
      return operand2 * operand1;
    default:
      printf("Unknown operator: %c\n", op);
      exit(1);
  }
}
