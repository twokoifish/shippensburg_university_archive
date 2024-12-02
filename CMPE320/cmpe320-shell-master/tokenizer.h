/**
 * @file tokenizer.h
 * @author Andrew Januszko & Joshua Burdette
 * @brief Holds all the prototypes for the tokenizer.
 * @version 0.1
 * @date 2021-03-29
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef TOKENIZER_H
#define TOKENIZER_H

#include <stdlib.h>
#include <string.h>
#include "cmd_list.h"

/**
 * @brief Create a new node of type `struct cmd` and add it to the end of the list.
 * 
 * @param word, the word in the node.
 * @param state, the state of the node.
 * @param command_line, the list to add the node to.
 */
void addNewNode(char *word, const enum machineState state, struct list_head *command_line);

/**
 * @brief Parse the buffer, break the input into nodes, and add them to the list.
 * 
 * @param command_line, the list of nodes.
 * @param buffer, the buffer of command line input.
 */
void convertToLabeledList(struct list_head *command_line, const char *buffer);

#endif /* TOKENIZER_H */