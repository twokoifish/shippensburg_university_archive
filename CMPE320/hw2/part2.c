/**
 * @file part2.c
 * @author Andrew Robert Januszko (aj8025@ship.edu)
 * @course CMPE 320 with Dr. Briggs
 * @brief : refactor of part 1, removed memory leaks.
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
enum argtype {
    WORD, DASH
};

/**
 * @brief : a struct that holds info about an argument like type and content.
 */
struct argument {
    enum argtype type;
    char *contents;
    struct list_head list;
};

/**
 * @brief : main method, sorts the command line input, then calls print.
 * 
 * @param argc : the amount of strings input.
 * @param argv : the strings input.
 * @return int : 0 if everything went okay, 1 if something went wrong.
 */
int main(int argc, char **argv)
{
  
  // creates the lists to hold the words and dashes.
  LIST_HEAD(list_words);
  LIST_HEAD(list_dashes);

  // for each string in the input, sort it into a list.
  for(int i = 1; i < argc; i++)
  {
    // allocate the space for the struct and store the input.
    struct argument *arg = malloc(sizeof(struct argument));
    arg->contents = strdup(argv[i]);

    // assign a type to the input and add it to a list.
    if (argv[i][0] == '-')
    {
      arg->type = DASH;
      list_add(&arg->list, &list_dashes);
    }else
    {
      arg->type = WORD;
      list_add(&arg->list, &list_words);
    }
  }

  // if the list is not empty, print the contents.
  if (!list_empty(&list_dashes))
  {
    printf("Arguments with dashes\n");
    list_print(&list_dashes);
  }

  // if the list is not empty, print the contents.
  if (!list_empty(&list_words))
  {
    printf("Arguments with words\n");
    list_print(&list_words);
  }

  // clear both lists
  clear_list(&list_dashes);
  clear_list(&list_words);

  return 0;
}

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