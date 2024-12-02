#include <stdio.h>
#include <stdlib.h>

typedef struct node{
    struct node *next;
    int data;
} node_t;


void traverse_linked_list(node_t *head);
void add_node(node_t* head, int data);


int main(void){

    // first element
    node_t *head;
    head = (node_t*)malloc(sizeof(node_t));
    head->next = NULL;
    head->data = 1;

    add_node(head, 2);
    add_node(head, 3);

    traverse_linked_list(head);

}



void traverse_linked_list(node_t *head){

    printf("The first node in linked list had data: %d\n", head->data);
    while(head->next != NULL){
        head = head->next;
        printf("Next node in linked list had data: %d\n", head->data);
    }
}

void add_node(node_t* head, int data){

    // create a new node to add to the end of the linked list
    node_t *new_node;
    new_node = (node_t*)malloc(sizeof(node_t));
    new_node->data = data;
    new_node->next = NULL; // NULL since it is at the end of the linked list

    while(head->next != NULL){
        head = head->next;
    }
    head->next = new_node; // add new node onto the end
}