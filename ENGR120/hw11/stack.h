#ifdef QUEUE
  #include "queue.h"
  typedef engr120_queue_t engr120_stack_t;
#else
  #include "linked_list.h"
  typedef node_t* engr120_stack_t;
#endif

/* Print the whole stack */
void show(engr120_stack_t s);

/* Initialize an empty stack */
engr120_stack_t init();

/* Pop the top element off the stack and return the element */
int pop(engr120_stack_t *s);

/* Push char c to the stack */
void push(engr120_stack_t *s, int c);

/* Return true if the stack is empty */
int is_empty(engr120_stack_t s);
