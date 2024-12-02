/**
 * @file list.c
 * @author Andrew Robert Januszko (aj8025@ship.edu)
 * @course CMPE 320 with Dr. Briggs
 * @brief : template for a doubly circular linked list.
 * @version 0.1
 * @date 2021-02-22
 * 
 * @copyright Copyright (c) 2021
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "list.h"

/**
 * @brief : enum of the data types for the struct.
 */
enum argtype
{
    WORD, DASH
};

/**
 * @brief : a struct that holds info about an argument like type and content.
 */
struct argument
{
    enum argtype type;
    char *contents;
    struct list_head list;
};

/**
 * @brief : prepends a node to a list.
 * 
 * @param new : the node to be prepends.
 * @param head : the list the node will be prepends to.
 */
void list_add(struct list_head *new, struct list_head *head)
{
  // store the first node in the head.
  struct list_head *next = head->next;

  // link the new node to the front of the list.
  next->prev = new;
  new->next = next;

  // link the new node to the end of the list to make it circular.
  new->prev = head;
  head->next = new;
}

/**
 * @brief : appends a node to a list.
 * 
 * @param new : the node to be appends.
 * @param head : the list the node will be appends to.
 */
void list_add_tail(struct list_head *new, struct list_head *head)
{
  // store the last node in the head.
  struct list_head *last_node = head->prev;

  // link the new node to the end of the list.
  last_node->next = new;
  new->prev = last_node;

  // link the new node to the front of the list to make it circular.
  new->next = head;
  head->prev = new;
}

/**
 * @brief : returns 0 if the list is not empty, 1 if it is.
 * 
 * @param head : the list to be checked.
 * @return int : 0 if the list is not empty, 1 if it is.
 */
int list_empty(struct list_head *head)
{
  return (head->next == head);
}

/**
 * @brief : prints the items in a list.
 * 
 * @param head : the list to be printed.
 */
void list_print(struct list_head *head)
{ 
  // create temp struct to hold the list and entry.
  struct list_head *curr;
  struct argument *entry;

  // for each node, get its entry and print the contents.
  for(curr = head->next; curr != head; curr = curr->next)
  {
    entry = list_entry(curr, struct argument, list);
    printf("%s\n", entry ->contents);
  }
}

/**
 * @brief : removes an entry from a list.
 * 
 * @param entry : the entry to be removed.
 */
void list_del(struct list_head *entry)
{
  // store the next and prev nodes for the entry.
  struct list_head *prev = entry->prev; 
  struct list_head *next = entry->next; 

  // link the previous and next nodes, removing entry.
  next->prev = prev; 
  prev->next = next;
}

/**
 * @brief : prepends the list to the head.
 * 
 * @param list : the list to be prepended.
 * @param head : the base list.
 */
void list_splice(struct list_head *list, struct list_head *head)
{
  // store the prev and next nodes from list.
  struct list_head *next_list = list->next;
	struct list_head *prev_list = list->prev;

  // store the head and next nodes from head.
	struct list_head *prev_head = head;
  struct list_head *next_head = head->next;

  // link head and next.
	next_list->prev = prev_head;
	prev_head->next = next_list;

  // make the list circular.
	prev_list->next = next_head;
	next_head->prev = prev_list;
}

/**
 * @brief : removes all nodes from the list.
 * 
 * @param list : the list to be cleared.
 */
void clear_list(struct list_head *list)
{
    // create a temp struct to hold the entry.
    struct argument *entry;

    // while the list is not empty, get the entry and remove it.
    while (!list_empty(list)) 
    {
      entry = list_entry(list->next, struct argument, list);
      list_del(&entry->list);
      free(entry->contents);
      free(entry);
    }
}

