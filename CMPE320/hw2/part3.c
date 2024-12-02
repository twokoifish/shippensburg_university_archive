/**
 * @file part3.c
 * @author Andrew Robert Januszko (aj8025@ship.edu)
 * @course CMPE 320 with Dr. Briggs
 * @brief : gets a list of rectangles, prints them, cleans up the list, then prints again.
 * @version 0.1
 * @date 2021-02-22
 * 
 * @copyright Copyright (c) 2021
 */
#include <stdio.h>
#include <stdlib.h>
#include "list.h"
#define MAX_CHAR_INPUT 50 // max amout of character input.
#define USER_INPUT_BREAK 0 // the sentinel value for the main while loop.
#define MIN_RECT_AREA_VALUE 10 // the minimum area of a rectangle.

/**
 * @brief : a struct that holds information about a rectangle.
 */
struct rectangle
{
  double length;
  double width;
  struct list_head list;
};

/**
 * @brief : get the length and width of a rectangle.
 * 
 * @param length : the length of the rectangle.
 * @param width  : the width of the rectangle.
 */
void getRectangle(double *length, double *width)
{
  printf("Enter a length and width: ");

  // create a buffer and pass the user input into it.
  char *buffer = malloc(MAX_CHAR_INPUT * sizeof(char));
  fgets(buffer, MAX_CHAR_INPUT, stdin);

  // parse the user input for the length and width.
  sscanf(buffer, "%lf %lf", length, width);

  // free the buffer.
  free(buffer);
}

/**
 * @brief : print all rectangles in a list.
 * 
 * @param head : the list of rectanlges.
 */
void printRectangles(struct list_head *head)
{
  // create temp structs to hold the list and rectangle.
  struct list_head *curr;
  struct rectangle *entry;

  // for each node, get its entry and print the length, width, and area.
  for(curr = head->next; curr != head; curr = curr->next)
  {
    // get the entry.
    entry = list_entry(curr, struct rectangle, list);

    // calculate the area.
    double area = entry->length * entry->width;

    // print the length, width, and area.
    printf("area %0.3f * %0.3f = %0.3f\n", entry->length, entry->width, area);
  }
}

/**
 * @brief : prints the total area of the rectangles in a list.
 * 
 * @param head : the list of rectangles.
 */
void printTotalArea(struct list_head *head)
{
  // create temp structs to hold the list and rectangle.
  struct list_head *curr;
  struct rectangle *entry;

  // create a variable to hold the net area.
  double netArea = 0;

  // for each node, get its entry and calculate the area.
  for(curr = head->next; curr != head; curr = curr->next)
  {
    // get the entry.
    entry = list_entry(curr, struct rectangle, list);

    // calculate the area and add it to the net area.
    netArea += entry->length * entry->width;
  }

  // print the net area of the rectangles.
  printf("Total area: %0.3f\n", netArea);
}

/**
 * @brief : removes the smallest rectangles in a list.
 * 
 * @param head : the list of rectangles.
 */
void sieveRectangles(struct list_head *head)
{
  printf("Removing small rectangles\n");

  // create temp structs to hold the list and rectangle.
  struct list_head *curr = head->next;
  struct rectangle *entry;

  // while the current node is not equal to the head, check it and remove if it's too small.
  while (curr != head)
  {
    // get the entry.
    entry = list_entry(curr, struct rectangle, list);

    // get the next node in the list.
    curr = curr->next;

    // calculate the area.
    double area = entry->length * entry->width;

    // if the area is too small, delete the node then free the node.
    if (area < MIN_RECT_AREA_VALUE)
    {
      // delete the node.
      list_del(&entry->list);

      // free the node.
      free(entry);
    }
  }
}

/**
 * @brief 
 * 
 * @return int 
 */
int main(void)
{

  // creates the list to hold the rectangles.
  LIST_HEAD(rectangle_list);

  // create variables to hold the length and width.
  double length, width;

  // do while length && width are not equal to 0.
  do {

    // get the length and width of a rectangle.
    getRectangle(&length, &width);

    // if the length && width are not equal to 0.
    if (length != USER_INPUT_BREAK && width != USER_INPUT_BREAK)
    {
      // create a new rectangle.
      struct rectangle *rect = malloc(sizeof(struct rectangle));

      // store the length and width.
      rect->length = length;
      rect->width = width;

      // add the rectangle to the list.
      list_add(&rect->list, &rectangle_list);
    } else
    {
      // length || width are equal to 0, exit the loop.
      break;
    }
  } while (length != USER_INPUT_BREAK && width != USER_INPUT_BREAK);

  // if the list of rectangles is not empty.
  if (!list_empty(&rectangle_list))
  {
    // print the list of rectangles.
    printRectangles(&rectangle_list);

    // print the total area of the rectangles.
    printTotalArea(&rectangle_list);

    // remove the small rectangles from the list.
    sieveRectangles(&rectangle_list);

    // print the new list of rectangles.
    printRectangles(&rectangle_list);

    // print the new total area of the rectangles.
    printTotalArea(&rectangle_list);
  }

  // clear the list of rectangles.
  clear_list(&rectangle_list);

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
    struct rectangle *entry;

    // while the list is not empty, get the entry and remove it.
    while (!list_empty(list)) 
    {
      entry = list_entry(list->next, struct rectangle, list);
      list_del(&entry->list);
      free(entry);
    }
}