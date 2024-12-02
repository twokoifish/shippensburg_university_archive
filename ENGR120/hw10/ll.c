/*
  Programmer: Andrew Januszko     Date of Creation: 20 April 2019
  Instructor: Chen Huo            Course: ENGR 120 - 03

  A external C file that contains all the functions required for "main_ll.c"
*/
#include <stdio.h> /* printf() definition. */
#include <stdlib.h> /* malloc(), free(), definitions. */
#include "linked_list.h" /* Function definitions. */

/* Make a single node with c as payload */
node_t *make_node(char c) {
  node_t *n = (node_t *)malloc(sizeof(node_t));
  n -> letter = c;
  n -> next = NULL;
  return n;
}

/* Make a list of characters with a string input and size */
node_t *make_list(char letters[], int n) {
  node_t *head = make_node(letters[0]);
  node_t *last = head;
  for(int i = 1; i < n; i++) {
    node_t *temp = make_node(letters[i]);
    last -> next = temp;
    last = temp;
  }
  return head;
}

/* Print the list. See an example output in the test sheet. */
void print_list(node_t *head) {
  if(head == NULL) {
    printf("NULL\n");
  } else {
    print_node(*head);
    print_list(head -> next);
  }
}

/* Return number of nodes */
int size(node_t *head) {
  node_t *temp = head;
  int number_nodes = 0;
  while (temp != NULL) {
    number_nodes++;
    temp = temp -> next;
  }
  return number_nodes;
}

/* Add a node with c as payload before the current head. Return the new head. */
node_t *prepend(node_t *head, char c) {
  node_t *extended = make_node(c);
  extended -> next = head;
  return extended;
}

/* Add a node at the end of list with payload c. */
node_t *append(node_t *head, char c) {
  node_t *temp = head;
  while (temp -> next != NULL) {
    temp = temp -> next;
  }
  insert_after(temp, c);
  return head;
}

/* Prints out the payload of a node in a single line */
void print_node(node_t n) {
  printf("%c -> ", n.letter);
}

/* Return a pointer to the ith element (0-indexed) */
node_t *get_ith(node_t *head, int pos) {
  for(int i = 0; i < pos - 1 && i < size(head); i++) {
    head = head -> next;
  }
  return head;
}

/* Make and insert a node after the ith node (0-indexed) */
node_t *insert_at(node_t *head, char c, int pos) {
  if(!pos) {
    return prepend(head, c);
  }
  insert_after(get_ith(head, pos), c);
  return head;
}

/* Make and insert a node after a node */
void insert_after(node_t *node, char c) {
  node_t *temp = make_node(c);
  temp -> next = node -> next;
  node -> next = temp;
}

/* Remove a node at the ith position (0-indexed) and free the space of the node */
node_t *remove_ith(node_t *head, int i) {
  node_t *temp = head;
  if(!i){
    head = temp -> next;
    free(temp);
    return head;
  }
  temp = get_ith(head, i);
  node_t *saved = temp -> next -> next;
  free(temp -> next);
  temp -> next = saved;
  return head;
}

/* Free the space of the nodes and set the head to NULL. */
void delete_list(node_t **head) {
  node_t *delete = *head;
  node_t *next;
  while(delete) {
    next = delete -> next;
    free(delete);
    delete = next;
  }
  *head = NULL;
}