/**
 * @file internal.c
 * @author Andrew Januszko & Joshua Burdette 
 * @brief Handles the environmental variables for the shell.
 * @version 0.1
 * @date 2021-03-31
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "internal.h"

/**
 * @brief allows the user to create/modify and environmental variable.
 * 
 * @param command_line the command line input
 * @param sushenv the environmental vars.
 * @return int 1 if successful.
 */
int handle_setenv(struct list_head *command_line, struct list_head *sushenv)
{
  struct cmd *key = list_entry(command_line->next->next, struct cmd, list);
  struct cmd *value = list_entry(command_line->next->next->next, struct cmd, list);
  char *temp = env_list_get_value(sushenv, key->word);
  if (temp == NULL)
  {
    struct envp *node = malloc(sizeof(struct envp));
    node->key = strdup(key->word);
    node->value = strdup(value->word);
    list_add_tail(&node->list, sushenv);
  }
  else
  {
    struct envp *node = env_get_variable(sushenv, key->word);
    node->value = strdup(value->word);
  }
  return 1;
}

/**
 * @brief Allows the user to get the value of an environmental var.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental vars.
 * @return int 1 if successful
 */
int handle_getenv(struct list_head *command_line, struct list_head *sushenv)
{
  struct cmd *entry = list_entry(command_line->next->next, struct cmd, list);
  char *value = env_list_get_value(sushenv, entry->word);
  if (entry->state == NULL_TERM)
  {
    struct list_head *curr = sushenv->next;
    while (curr != sushenv)
    {
      struct envp *env = list_entry(curr, struct envp, list);
      curr = curr->next;
      printf("%s=%s\n", env->key, env->value);
    }
  }
  else if (value == NULL)
  {
    fprintf(stderr, ERROR_GETENV_INVALID, entry->word);
  }
  else
  {
    printf("%s\n", value);
  }
  return 1;
}

/**
 * @brief Allows the user to delete environmental vars.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental vars.
 * @ruretn int 1 if successful.
 */
int handle_unsetenv(struct list_head *command_line, struct list_head *sushenv)
{
  struct cmd *entry = list_entry(command_line->next->next, struct cmd, list);
  struct envp *value = env_get_variable(sushenv, entry->word);
  if (value != NULL)
  {
    list_del(&value->list);
    free(value->key);
    free(value->value);
    free(value);
  }
  return 1;
}

/**
 * @brief Handles the ability to change directories.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental vars.
 * @return int 1 if successful.
 */
int handle_cd(struct list_head *command_line, struct list_head *sushenv)
{
  struct cmd *cd = list_entry(command_line->next, struct cmd, list);
  struct list_head *args = command_line->next;
  struct cmd *endordir = list_entry(args->next, struct cmd, list);
  if (endordir->state != NULL_TERM)
  {
    if (chdir(endordir->word) != 0)
    {
      perror(endordir->word);
    }
    return 1;
  }
  else
  {
    struct envp *node = env_get_variable(sushenv, "HOME");
    chdir(node->value);
  }
  return 1;
}

/**
 * @brief prints the working directory of the program.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental vars.
 * @return int 1 if successful.
 */
int handle_pwd(struct list_head *command_line, struct list_head *sushenv)
{
  char workingDirectory[MAX_PATH_LENGTH];
  getcwd(workingDirectory, sizeof(workingDirectory));
  printf("%s\n", workingDirectory);
  return 1;
}

/**
 * @brief allows the user to exit the shell.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental vars.
 * @return int 1 if successful.
 */
int handle_exit(struct list_head *command_line, struct list_head *sushenv)
{
  cmd_list_clear(command_line);
  env_list_clear(sushenv);
  exit(0);
}

/**
 * @brief checks to see if a command is an internal one.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental varaibles.
 * @return int 1 if successful.
 */
int handle_internal_command(struct list_head *command_line, struct list_head *sushenv)
{
  int index = 0;
  struct cmd *command = list_entry(command_line->next, struct cmd, list);
  while (internal_commands[index].name != 0)
  {
    if (!strcmp(internal_commands[index].name, command->word))
    {
      return internal_commands[index].handler(command_line, sushenv);
    }
    index++;
  }
  return 0;
}
