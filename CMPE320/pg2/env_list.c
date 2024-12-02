/**
 * @file env_list.c
 * @author Andrew Januszko & Joshua Burdette 
 * @brief the functionality for our environmental variables list.
 * @version 0.1
 * @date 2021-03-31
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "env_list.h"

/**
 * @brief prints all the environmental variables.
 * 
 * @param sushenvp the environmental variables.
 */
void env_list_print(struct list_head *sushenvp)
{
  struct list_head *curr = sushenvp->next;
  while (curr != sushenvp)
  {
    struct envp *env = list_entry(curr, struct envp, list);
    curr = curr->next;
    printf("%s=%s\n", env->key, env->value);
  }
}

/**
 * @brief clears all the environmental variables.
 * 
 * @param sushenvp the environmental variables.
 */
void env_list_clear(struct list_head *sushenvp)
{
  struct envp *entry;

  while (!list_empty(sushenvp))
  {
    entry = list_entry(sushenvp->next, struct envp, list);
    list_del(&entry->list);
    free(entry->key);
    free(entry->value);
    free(entry);
  }
}

/**
 * @brief builds the list of the environmental variables.
 * 
 * @param sushenvp the environmental variables.
 * @param envp the command line variables.
 */
void env_list_build(struct list_head *sushenvp, char **envp)
{
  int i = 0;
  while (envp[i] != NULL)
  {
    struct envp *node = malloc(sizeof(struct envp));
    int starting_index = 0;
    while (envp[i][starting_index] != '=')
    {
      starting_index++;
    }
    node->key = strndup(envp[i], starting_index);
    starting_index++;
    node->value = strndup(&envp[i][starting_index], strlen(envp[i]) - starting_index);
    list_add(&node->list, sushenvp);
    i = i + 1;
  }
}

/**
 * @brief counts the environmental variables.
 * 
 * @param sushenvp the environmental variables.
 * @return int the count.
 */
int env_list_count_entries(struct list_head *sushenvp)
{
  struct list_head *curr;
  int count = 0;
  for (curr = sushenvp->next; curr->next != sushenvp->next; curr = curr->next)
  {
    count = count + 1;
  }
  return count;
}

/**
 * @brief gets the value of the environmental variables.
 * 
 * @param sushenvp the environmental variables.
 * @param key the variable you want to check.
 * @return char* the value or null, depending on if it exists.
 */
char *env_list_get_value(struct list_head *sushenvp, const char *key)
{
  struct list_head *curr = sushenvp->next;
  while (curr != sushenvp)
  {
    struct envp *node = list_entry(curr, struct envp, list);
    curr = curr->next;
    if (!strcmp(node->key, key))
    {
      char *envVar = malloc(1024 * sizeof(char));
      strcpy(envVar, node->key);
      strcat(envVar, "=");
      strcat(envVar, node->value);
      return envVar;
    }
  }
  return NULL;
}

/**
 * @brief gets a specific environmental varaible.
 * 
 * @param sushenvp the environmental variables.
 * @param key they key.
 * @return struct envp* the variable.
 */
struct envp *env_get_variable(struct list_head *sushenvp, const char *key)
{
  struct list_head *curr = sushenvp->next;
  while (curr != sushenvp)
  {
    struct envp *node = list_entry(curr, struct envp, list);
    curr = curr->next;
    if (!strcmp(node->key, key))
    {
      return node;
    }
  }
  return NULL;
}

/**
 * @brief gets the PS1 var from the environmental variables.
 * 
 * @param sushenvp the environmental variables.
 * @return char* the ps1.
 */
char *check_PS1(struct list_head *sushenvp)
{
  struct envp *node = env_get_variable(sushenvp, "PS1");
  if (node != NULL)
  {
    printf("%s", node->value);
  }
  else
  {
    printf(">");
  }
}

/**
 * @brief converts the linked list of environmental variables to an array.
 * 
 * @param sushenvp the environmental variables.
 * @param environment_vars the array.
 */
void env_convert_to_list(struct list_head *sushenvp, char **environment_vars)
{
  struct list_head *curr;
  struct envp *entry;
  int i = 0;
  for (curr = sushenvp->next; curr->next != sushenvp->next; curr = curr->next)
  {
    entry = list_entry(curr, struct envp, list);
    environment_vars[i] = malloc(1024 * sizeof(char *));
    strcpy(environment_vars[i], entry->key);
    strcat(environment_vars[i], "=");
    strcat(environment_vars[i], entry->value);
    i++;
  }
  environment_vars[i] = (char *)0;
}
