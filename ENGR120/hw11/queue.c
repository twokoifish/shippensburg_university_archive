#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include "queue.h"

engr120_queue_t queue_init() {
  engr120_queue_t queue;
  queue.head = NULL;
  queue.tail = NULL;
  return queue;
}

/* removeith(lst, size(lst) - 1) */
int dequeue(engr120_queue_t *q) {
  if(is_empty_queue(*q) == 1) {
    return NULL;
  }

}

/* prepend */
void enqueue(engr120_queue_t *q, int i) {
  engr120_queue_t temp = queue_init();
  temp.head -> number = i;
  if(q -> tail == NULL) {
    q -> head = q -> tail = temp.head;
    return;
  }
  q -> tail -> next = temp.head;
  q -> tail = temp.head;
}

int is_empty_queue(engr120_queue_t q) {
  return (q.head == NULL);
}

void print_queue(engr120_queue_t q) {
  engr120_queue_t temp = q;
	while(temp.head != NULL) {
		printf("%d ",temp.head->number);
		temp.head = temp.head->next;
	}
	printf("\n");
}

/* You can use this main as a test.
 * gcc -o queue queue.o ll.o
 */

int main(void) {
  engr120_queue_t q = queue_init();
  engr120_queue_t *queue = &q;

  enqueue(queue, 1);
  print_queue(*queue);
  enqueue(queue, 2);
  print_queue(*queue);
  enqueue(queue, 3);
  print_queue(*queue);
  enqueue(queue, 4);
  print_queue(*queue);

  int d1 = dequeue(queue);
  print_queue(*queue);

  return (0);
}
