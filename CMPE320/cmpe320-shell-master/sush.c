/**
 * @file sush.c
 * @author Andrew Januszko & Joshua Burdette 
 * @brief The main file for the shell.
 * @version 0.1
 * @date 2021-03-31
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "sush.h"

/**
 * @brief Creates the environmental variables and then starts the program.
 * 
 * @param arg number of command line args
 * @param argv the command line args
 * @param envp the command line environmental variables.
 * @return int 0 if everything went well.
 */
int main(int arg, char **argv, char **envp)
{
  LIST_HEAD(sushenvp);
  env_list_build(&sushenvp, envp);
  char *buffer = calloc(MAX_CHAR_INPUT, sizeof(char));
  run(&sushenvp, buffer);
  return 0;
}

/**
 * @brief runs the parser and execution for the shell.
 * 
 * @param sushenvp the command line environmental variables.
 * @param buffer the command line input.
 */
void run(struct list_head *sushenvp, char *buffer)
{
  check_PS1(sushenvp);
  fflush(stdout);
  while (fgets(buffer, MAX_CHAR_INPUT + 1, stdin) != NULL)
  {
    if (buffer[0] != '\n' && buffer[0] != ' ')
    {
      LIST_HEAD(command_line);
      convertToLabeledList(&command_line, buffer);
      execute(&command_line, sushenvp);
      cmd_list_clear(&command_line);
    }
    check_PS1(sushenvp);
    fflush(stdout);
  }
}
