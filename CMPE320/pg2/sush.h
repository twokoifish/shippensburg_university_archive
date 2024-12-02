/**
 * @file sush.h
 * @author Andrew Januszko & Joshua Burdette 
 * @brief Header file for sush.h
 * @version 0.1
 * @date 2021-03-31
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef SUSH_H
#define SUSH_H

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "env_list.h"
#include "tokenizer.h"
#include "execute.h"

#define MAX_CHAR_INPUT 1024

/**
 * @brief runs the parser and execution for the shell.
 * 
 * @param sushenvp the command line environmental variables.
 * @param buffer the command line input.
 */
void getCLIFromUser(char *buffer);
void run(struct list_head *sushenvp, char *buffer);

#endif /* SUSH_H */