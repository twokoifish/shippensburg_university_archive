/**
 * @file tokenizer.c
 * @author Andrew Januszko & Joshua Burdette 
 * @brief Converts user input into a list of nodes.
 * @version 0.1
 * @date 2021-03-31
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "tokenizer.h"

/**
 * @brief Create a new node of type `struct cmd` and add it to the end of the list.
 * 
 * @param word, the word in the node.
 * @param state, the state of the node.
 * @param command_line, the list to add the node to.
 */
void addNewNode(char *word, const enum machineState state, struct list_head *command_line)
{
  struct cmd *node = malloc(sizeof(struct cmd));
  node->word = word;
  node->state = state;
  list_add_tail(&node->list, command_line);
}

/**
 * @brief Parse the buffer, break the input into nodes, and add them to the list.
 * 
 * @param command_line, the list of nodes.
 * @param buffer, the buffer of command line input.
 */
void convertToLabeledList(struct list_head *command_line, const char *buffer)
{

  size_t buffer_len = strlen(buffer);
  int starting_index = 0;
  enum machineState currentState = START;

  for (int i = 0; i < buffer_len; i++)
  {
    switch (currentState)
    {

    case START:
      if (buffer[i] == ' ')
      {
        starting_index = i;
        currentState = START;
      }
      else if (buffer[i] == '\t')
      {
        starting_index = i;
        currentState = START;
      }
      else if (buffer[i] == '\n' || buffer[i] == '\r')
      {
        i--;
        currentState = NULL_TERM;
      }
      else
      {
        starting_index = i;
        currentState = COMMAND;
      }
      break;

    case COMMAND:
      if (buffer[i] == ' ')
      {
        addNewNode(strndup(&buffer[starting_index], i - starting_index), COMMAND, command_line);
        starting_index = i;
        currentState = WHITESPACE;
      }
      else if (buffer[i] == '\n' || buffer[i] == '\r')
      {
        addNewNode(strndup(&buffer[starting_index], i - starting_index), COMMAND, command_line);
        starting_index = i;
        currentState = NULL_TERM;
        i--;
      }
      break;

    case ARGUMENTS:
      if (buffer[i] == ' ')
      {
        addNewNode(strndup(&buffer[starting_index], i - starting_index), ARGUMENTS, command_line);
        starting_index = i;
        currentState = WHITESPACE;
      }
      else if (buffer[i] == '\n' || buffer[i] == '\r')
      {
        addNewNode(strndup(&buffer[starting_index], i - starting_index), ARGUMENTS, command_line);
        starting_index = i;
        currentState = NULL_TERM;
        i--;
      }
      else if (buffer[i] == '.')
      {
        currentState = FILE_NAME;
      }
      break;

    case FILE_NAME:
      if (buffer[i] == ' ')
      {
        addNewNode(strndup(&buffer[starting_index], i - starting_index), FILE_NAME, command_line);
        starting_index = i;
        currentState = WHITESPACE;
      }
      else if (buffer[i] == '\n' || buffer[i] == '\r')
      {
        addNewNode(strndup(&buffer[starting_index], i - starting_index), FILE_NAME, command_line);
        starting_index = i;
        currentState = NULL_TERM;
        i--;
      }
      break;

    case PIPE:
      if (currentState == PIPE)
      {
        addNewNode(strndup(&buffer[starting_index], i - starting_index), PIPE, command_line);
        starting_index = i;
        currentState = START;
      }
      break;

    case WHITESPACE:
      if (buffer[i] == ' ')
      {
        starting_index = i;
      }
      else if (buffer[i] == '\"' || buffer[i] == '\'')
      {
        starting_index = i + 1;
        currentState = QUOTE;
      }
      else if (buffer[i] == '\n' || buffer[i] == '\r')
      {
        starting_index = i;
        currentState = NULL_TERM;
        i--;
      }
      else if (buffer[i] == '|')
      {
        addNewNode("NULL", NULL_TERM, command_line);
        starting_index = i;
        currentState = PIPE;
      }
      else if (buffer[i] == '>' && buffer[i + 1] == '>')
      {
        addNewNode("NULL", NULL_TERM, command_line);
        starting_index = i;
        currentState = APPEND;
      }
      else if (buffer[i] == '>')
      {
        addNewNode("NULL", NULL_TERM, command_line);
        starting_index = i;
        currentState = STDOUT;
      }
      else if (buffer[i] == '<')
      {
        addNewNode("NULL", NULL_TERM, command_line);
        starting_index = i;
        currentState = STDIN;
      }
      else if (buffer[i] == '.' || buffer[i] == '/')
      {
        starting_index = i;
        currentState = FILE_NAME;
      }
      else
      {
        starting_index = i;
        currentState = ARGUMENTS;
      }
      break;

    case QUOTE:
      if (buffer[i] == '\"' || buffer[i] == '\'')
      {
        addNewNode(strndup(&buffer[starting_index], i - (starting_index)), QUOTE, command_line);
        currentState = WHITESPACE;
      }
      break;

    case STDOUT:
      if (buffer[i] == ' ')
      {
        addNewNode(strndup(&buffer[starting_index], i - starting_index), STDOUT, command_line);
        starting_index = i;
        currentState = WHITESPACE;
      }
      break;

    case STDIN:
      if (buffer[i] == ' ')
      {
        addNewNode(strndup(&buffer[starting_index], i - starting_index), STDIN, command_line);
        starting_index = i;
        currentState = WHITESPACE;
      }
      break;

    case APPEND:
      if (buffer[i] == ' ')
      {
        addNewNode(strndup(&buffer[starting_index], i - starting_index), APPEND, command_line);
        starting_index = i;
        currentState = WHITESPACE;
      }
      break;

    case NULL_TERM:
      if (currentState == NULL_TERM)
      {
        addNewNode("NULL", NULL_TERM, command_line);
      }
      break;
    }
  }
}
