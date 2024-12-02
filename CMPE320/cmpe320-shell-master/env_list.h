/**
 * @file env_list.h
 * @author Andrew Januszko & Joshua Burdette 
 * @brief Header file for env_list.c
 * @version 0.1
 * @date 2021-03-31
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef ENV_LIST_H
#define ENV_LIST_H

#include "list.h"

/**
 * @brief Struct to hold variable information.
 * 
 */
struct envp {
  char *key;
  char *value;
  struct list_head list;
};

/**
 * @brief prints all the environmental variables.
 * 
 * @param sushenvp the environmental variables.
 */
void env_list_print(struct list_head *sushenvp);

/**
 * @brief clears all the environmental variables.
 * 
 * @param sushenvp the environmental variables.
 */
void env_list_clear(struct list_head *sushenvp);

/**
 * @brief builds the list of the environmental variables.
 * 
 * @param sushenvp the environmental variables.
 * @param envp the command line variables.
 */
void env_list_build(struct list_head *sushenvp, char **envp);

/**
 * @brief counts the environmental variables.
 * 
 * @param sushenvp the environmental variables.
 * @return int the count.
 */
int env_list_count_entries(struct list_head *sushenvp);

/**
 * @brief gets the value of the environmental variables.
 * 
 * @param sushenvp the environmental variables.
 * @param key the variable you want to check.
 * @return char* the value or null, depending on if it exists.
 */
char *env_list_get_value(struct list_head *sushenvp, const char *key);

/**
 * @brief gets the PS1 var from the environmental variables.
 * 
 * @param sushenvp the environmental variables.
 * @return char* the ps1.
 */
char *check_PS1(struct list_head *sushenvp);

/**
 * @brief gets a specific environmental varaible.
 * 
 * @param sushenvp the environmental variables.
 * @param key they key.
 * @return struct envp* the variable.
 */
struct envp *env_get_variable(struct list_head *sushenvp, const char *key);

/**
 * @brief converts the linked list of environmental variables to an array.
 * 
 * @param sushenvp the environmental variables.
 * @param environment_vars the array.
 */
void env_convert_to_list(struct list_head *sushenvp, char **environment_vars);

#endif /* ENV_LIST_H */