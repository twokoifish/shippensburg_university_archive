/**
 * @file cmd_list.h
 * @author Andrew Januszko & Joshua Burdette 
 * @brief Header file for cmd_list.c, holds prototype functions.
 * @version 0.1
 * @date 2021-03-29
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef CMD_LIST_H
#define CMD_LIST_H

#include "list.h"
#include <string.h>

/**
 * @brief enum of all supported states.
 * 
 */
enum machineState
{
  START,
  COMMAND,
  ARGUMENTS,
  FILE_NAME,
  PIPE,
  WHITESPACE,
  QUOTE,
  STDOUT,
  STDIN,
  APPEND,
  NULL_TERM
};

/**
 * @brief struct defining a `struct cmd` node for a list.
 * 
 */
struct cmd
{
  char *word;
  enum machineState state;
  struct list_head list;
};

/**
 * @brief Print each node in a list of type `struct cmd`.
 * 
 * @param commands, the list of nodes.
 */
void cmd_list_print(struct list_head *commands);

/**
 * @brief Clear each node in a list of type `struct cmd`.
 * 
 * @param commands, the list of nodes.
 */
void cmd_list_clear(struct list_head *commands);

/**
 * @brief Counts the number of pipes in the list.
 * 
 * @param commands, the list to count.
 * @return int, the number of pipes in the list.
 */
int cmd_count_pipes(struct list_head *commands);

/**
 * @brief Count the number of commands in a list.
 * 
 * @param commands the list.
 * @return int the number of commands.
 */
int cmd_count_command_nodes(struct list_head *commands);

/**
 * @brief Converts all commands in a list to an array.
 * 
 * @param commands the list.
 * @param args the array.
 */
void cmd_fetch_command(struct list_head *commands, char **args);

/**
 * @brief Gets the total number of nodes in a list.
 * 
 * @param commands the list.
 * @return int the count of nodes.
 */
int cmd_count_total_nodes(struct list_head *commands);

#endif /* CMD_LIST_H */