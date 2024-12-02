/**
 * @file execute.h
 * @author Andrew Januszko & Joshua Burdette 
 * @brief Header for execute.c
 * @version 0.1
 * @date 2021-03-31
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef EXECUTE_H
#define EXECUTE_H

#include "internal.h"
#include "tokenizer.h"
#include "env_list.h"
#include "list.h"
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include "error_msgs.h"

/**
 * @brief executes a single command.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental variables.
 */
void execute_command(struct list_head *command_line, struct list_head *sushenv);

/**
 * @brief Allows the use to recursively execute commands.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental variables.
 * @param pipesRemaing the number of pipes left in the input.
 */
void execute_recursive(struct list_head *command_line, struct list_head *sushenv, int pipesRemaing);

/**
 * @brief Checks to see if something is an internal or external command or if it has pipes.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental variables.
 */
void execute(struct list_head *command_line, struct list_head *sushenv);

/**
 * @brief Moves the starting position of the pipe.
 * 
 * @param token_list the list of tokens
 * @param start_position the starting position.
 * @param command_line_length the length of the command.
 * @param pipe_next if there is a pipe next.
 * @return int the next position.
 */
int move_start_position_and_check_pipe(struct list_head *token_list, int start_position, int command_line_length, int *pipe_next);

/**
 * @brief Convers a node to an array.
 * 
 * @param token_list the list of tokens.
 * @param start_node the starting position.
 * @param command_line_size the number of nodes to take.
 * @return char** the array.
 */
char **change_list_to_array(struct list_head *token_list, int start_node, int command_line_size);

/**
 * @brief Get the nodes in command object
 * 
 * @param token_list the list of tokens.
 * @param start_node the starting position.
 * @return int the number of nodes.
 */
int get_nodes_in_command(struct list_head *token_list, int start_node);

/**
 * @brief Gets the size of the entire list.
 * 
 * @param list the list.
 * @return int the size.
 */
int list_size(struct list_head *list);

#endif /* EXECUTE_H */