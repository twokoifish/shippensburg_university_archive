/**
 * @file internal.h
 * @author Andrew Januszko & Joshua Burdette 
 * @brief Header for internal.c
 * @version 0.1
 * @date 2021-03-27
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef INTERNAL_H
#define INTERNAL_H

#define MAX_PATH_LENGTH 1024 // the maximum size of a path
#include <unistd.h>
#include "env_list.h"
#include "tokenizer.h"
#include "error_msgs.h"
#include <limits.h>

/**
 * @brief allows us to define handlers for all internal commands.
 * 
 */
typedef struct internal
{
  const char *name;
  int (*handler)(struct list_head *command_line, struct list_head *sushenv);
} internal_t;

/**
 * @brief allows the user to create/modify and environmental variable.
 * 
 * @param command_line the command line input
 * @param sushenv the environmental vars.
 * @return int 1 if successful.
 */
int handle_setenv(struct list_head *command_line, struct list_head *sushenv);

/**
 * @brief Allows the user to get the value of an environmental var.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental vars.
 * @return int 1 if successful
 */
int handle_getenv(struct list_head *command_line, struct list_head *sushenv);

/**
 * @brief Allows the user to delete environmental vars.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental vars.
 * @ruretn int 1 if successful.
 */
int handle_unsetenv(struct list_head *command_line, struct list_head *sushenv);

/**
 * @brief Handles the ability to change directories.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental vars.
 * @return int 1 if successful.
 */
int handle_cd(struct list_head *command_line, struct list_head *sushenv);

/**
 * @brief prints the working directory of the program.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental vars.
 * @return int 1 if successful.
 */
int handle_pwd(struct list_head *command_line, struct list_head *sushenv);

/**
 * @brief allows the user to exit the shell.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental vars.
 * @return int 1 if successful.
 */
int handle_exit(struct list_head *command_line, struct list_head *sushenv);

/**
 * @brief Holds all the internal commands.
 * 
 */
static internal_t internal_commands[] = {
    {.name = "setenv", .handler = handle_setenv},
    {.name = "getenv", .handler = handle_getenv},
    {.name = "unsetenv", .handler = handle_unsetenv},
    {.name = "cd", .handler = handle_cd},
    {.name = "pwd", .handler = handle_pwd},
    {.name = "exit", .handler = handle_exit},
    0};

/**
 * @brief checks to see if a command is an internal one.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental varaibles.
 * @return int 1 if successful.
 */
int handle_internal_command(struct list_head *command_line, struct list_head *sushenv);

#endif /* INTERNAL_H */