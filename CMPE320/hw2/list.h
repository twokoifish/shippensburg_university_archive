/**
 * @file list.h
 * @author Andrew Robert Januszko (aj8025@ship.edu)
 * @course CMPE 320 with Dr. Briggs
 * @brief : template for a doubly circular linked list.
 * @version 0.1
 * @date 2021-02-22
 * 
 * @copyright Copyright (c) 2021
 */
#ifndef LIST_H
#define LIST_H

#include <stddef.h>

/**
 * @brief: macro for initializing list_head. Initializes its previous and next nodes to itself.
 * 
 * @param name: the list to initialize.
 */
#define LIST_HEAD_INIT(name) { &(name), &(name) }

/**
 * @brief: macro for creating list_head. Creates a new list_head struct using LIST_HEAD_INIT.
 * 
 * @param name: the list to create.
 */
#define LIST_HEAD(name)\
  struct list_head name = LIST_HEAD_INIT(name)

/**
 * @brief: fetches the struct wrapped around a node.
 * 
 * @param ptr: the node to use.
 * @param type: the type of struct wrapped around the node.
 * @param member: the member referenced by the wrapper.
 */
#define list_entry(ptr, type, member)({ \
   void *__mptr = (void *)(ptr);        \
   ((type *)(__mptr - offsetof(type, member)));  \
})


/**
 * @brief: struct holding prev and next list_heads.
 * 
 * @param prev: the previous list_head.
 * @param next: the next list_head.
 */
struct list_head {
  struct list_head *prev;
  struct list_head *next;
};

/**
 * @brief : prepends a node to a list.
 * 
 * @param new : the node to be prepends.
 * @param head : the list the node will be prepends to.
 */
void list_add(struct list_head *new, struct list_head *head);

/**
 * @brief : appends a node to a list.
 * 
 * @param new : the node to be appends.
 * @param head : the list the node will be appends to.
 */
void list_add_tail(struct list_head *new, struct list_head *head);

/**
 * @brief : removes an entry from a list.
 * 
 * @param entry : the entry to be removed.
 */
void list_del(struct list_head *entry);

/**
 * @brief : returns 0 if the list is not empty, 1 if it is.
 * 
 * @param head : the list to be checked.
 * @return int : 0 if the list is not empty, 1 if it is.
 */
int list_empty(struct list_head *head);

/**
 * @brief : prepends the list to the head.
 * 
 * @param list : the list to be prepended.
 * @param head : the base list.
 */
void list_splice(struct list_head *list, struct list_head *head);

/**
 * @brief : removes all nodes from the list.
 * 
 * @param list : the list to be cleared.
 */
void clear_list(struct list_head *list);

/**
 * @brief : prints the items in a list.
 * 
 * @param head : the list to be printed.
 */
void list_print(struct list_head *head);

#endif /* LIST_H */