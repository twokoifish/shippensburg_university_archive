#include "linked_list.h"

typedef struct {
  node_t *head;
  node_t *tail;
} engr120_queue_t;

engr120_queue_t queue_init();

/* remove the "head" of the queue */
int dequeue(engr120_queue_t *q);

/* add an element at the end of the queue */
void enqueue(engr120_queue_t *q, int i);

int is_empty_queue(engr120_queue_t q);

/* See the format in the test sheet */
void print_queue(engr120_queue_t q);
