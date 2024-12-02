/**
 * @file cmd_list.c
 * @author Andrew Januszko & Joshua Burdette
 * @brief Holds the functions specific to `struct cmd` lists.
 * @version 0.1
 * @date 2021-03-29
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "cmd_list.h"

/**
 * @brief Print each node in a list of type `struct cmd`.
 * 
 * @param commands, the list of nodes.
 */
void cmd_list_print(struct list_head *commands)
{
  struct list_head *curr;
  struct cmd *entry;

  for (curr = commands->next; curr != commands; curr = curr->next)
  {
    entry = list_entry(curr, struct cmd, list);
    printf("CMD: %d, %s\n", entry->state, entry->word);
  }
}

/**
 * @brief Clear each node in a list of type `struct cmd`.
 * 
 * @param commands, the list of nodes.
 */
void cmd_list_clear(struct list_head *commands)
{
  while (!list_empty(commands))
  {
    struct list_head *curr = commands->next;
    struct cmd *entry = list_entry(curr, struct cmd, list);
    list_del(&entry->list);
    if (strcmp(entry->word, "NULL"))
    {
      free(entry->word);
    }
    free(entry);
  }
}

/**
 * @brief Counts the number of pipes in the list.
 * 
 * @param commands, the list to count.
 * @return int, the number of pipes in the list.
 */
int cmd_count_pipes(struct list_head *commands) {
  struct list_head *curr;
  struct cmd *entry;
  int pipeCount = 0;

  for (curr = commands->next; curr->next != commands->next; curr = curr->next)
  {
      entry = list_entry(curr, struct cmd, list);

      if (entry->state == PIPE) {
        pipeCount++;
      }
  }

  return pipeCount;
}

/**
 * @brief Count the number of commands in a list.
 * 
 * @param commands the list.
 * @return int the number of commands.
 */
int cmd_count_command_nodes(struct list_head *commands) {
  struct list_head *curr;
  struct cmd *entry;
  int pipeCount = 0;

  for (curr = commands->next; curr->next != commands->next; curr = curr->next)
  {
      entry = list_entry(curr, struct cmd, list);

      if (entry->state == NULL_TERM) {
        return pipeCount;
      }

      pipeCount++;
  }

  return pipeCount;
}

/**
 * @brief Converts all commands in a list to an array.
 * 
 * @param commands the list.
 * @param args the array.
 */
void cmd_fetch_command(struct list_head *commands, char **args) {

 struct list_head *curr;
  struct cmd *entry;

  int i = 0;
  for (curr = commands->next; curr->next != commands->next; curr = curr->next)
  {
    entry = list_entry(curr, struct cmd, list);
    
    args[i] = malloc(1024 * sizeof(char *));
    if (entry->state == NULL_TERM) {
      args[i] = (char *)0;
    } else {
      args[i] = entry->word;
    }
    i++;
  }
}

/**
 * @brief Gets the total number of nodes in a list.
 * 
 * @param commands the list.
 * @return int the count of nodes.
 */
int cmd_count_total_nodes(struct list_head *commands) {
  struct list_head *curr;
  struct cmd *entry;
  int count = 0;

  for (curr = commands->next; curr->next != commands->next; curr = curr->next)
  {
      count++;
  }

  return count;
}
