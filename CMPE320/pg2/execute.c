/**
 * @file execute.c
 * @author Andrew Januszko & Joshua Burdette 
 * @brief Allows the user to execute commands.
 * @version 0.1
 * @date 2021-03-31
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "execute.h"

/**
 * @brief Gets the size of the entire list.
 * 
 * @param list the list.
 * @return int the size.
 */
int list_size(struct list_head *list)
{
  int size = 0;
  for (struct list_head *tok = list->next; tok != list; tok = tok->next)
  {
    size++;
  }
  return size;
}

/**
 * @brief Get the nodes in command object
 * 
 * @param token_list the list of tokens.
 * @param start_node the starting position.
 * @return int 
 */
int get_nodes_in_command(struct list_head *token_list, int start_node)
{
  int size = 0;
  int i = 0;
  for (struct list_head *tok = token_list->next; tok != token_list; tok = tok->next)
  {
    struct cmd *l_entry = list_entry(tok, struct cmd, list);
    if (i >= start_node)
    {
      if (l_entry->state == NULL_TERM)
        break;
      size++;
    }
    i++;
  }
  return size;
}

/**
 * @brief Convers a node to an array.
 * 
 * @param token_list the list of tokens.
 * @param start_node the starting position.
 * @param command_line_size the number of nodes to take.
 * @return char** the array.
 */
char **change_list_to_array(struct list_head *token_list, int start_node, int command_line_size)
{
  char **array_of_tokens = calloc(sizeof(char *), command_line_size);
  int i = 0;
  int j = 0;
  for (struct list_head *tok = token_list->next; /*tok != token_list;*/ i < start_node + command_line_size; tok = tok->next)
  {
    struct cmd *l_entry = list_entry(tok, struct cmd, list);
    if (i >= start_node)
    {
      array_of_tokens[j] = l_entry->word;
      j++;
    }
    i++;
  }
  return array_of_tokens;
}

/**
 * @brief Moves the starting position of the pipe.
 * 
 * @param token_list the list of tokens
 * @param start_position the starting position.
 * @param command_line_length the length of the command.
 * @param pipe_next if there is a pipe next.
 * @return int the next position.
 */
int move_start_position_and_check_pipe(struct list_head *token_list, int start_position, int command_line_length, int *pipe_next)
{
  int i = 0;
  for (struct list_head *tok = token_list->next; tok != token_list; tok = tok->next)
  {
    struct cmd *l_entry = list_entry(tok, struct cmd, list);
    if (i <= start_position)
    {
      i++;
      if (l_entry->state == NULL_TERM)
      { //wont work with redirs
        break;
      }
    }
  }
  return i + 1;
}

/**
 * @brief executes a single command.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental variables.
 */
void execute_command(struct list_head *command_line, struct list_head *sushenv)
{
  pid_t pid = fork();
  if (pid == 0)
  {
    struct list_head *curr;
    struct cmd *entry;
    int output = -1;
    int input = -1;
    int hasout = 0;
    int hasin = 0;
    for (curr = command_line->next; curr->next != command_line->next; curr = curr->next)
    {
      entry = list_entry(curr, struct cmd, list);
      if (!strcmp(entry->word, ">"))
      {
        hasout++;
        if (hasout > 1)
        {
          fprintf(stderr, ERROR_INVALID_CMDLINE);
          return;
        }
        curr = curr->next;
        entry = list_entry(curr, struct cmd, list);

        FILE *fp = fopen(entry->word, "wr");
        output = open(entry->word, O_WRONLY | O_CREAT);
      }
      else if (!strcmp(entry->word, ">>"))
      {
        curr = curr->next;
        entry = list_entry(curr, struct cmd, list);
        FILE *fp = fopen(entry->word, "a");
        output = open(entry->word, O_APPEND | O_WRONLY | O_CREAT);
        if (hasout == 1)
        {
          fprintf(stderr, ERROR_INVALID_CMDLINE);
          return;
        }
        else
        {
          hasout = 1;
        }
      }
      else if (!strcmp(entry->word, "<"))
      {
        curr = curr->prev->prev;
        entry = list_entry(curr, struct cmd, list);
        FILE *fp = fopen(entry->word, "r");
        input = open(entry->word, O_RDONLY);
        curr = curr->next->next;
        entry = list_entry(curr, struct cmd, list);
        if (hasin == 1)
        {
          fprintf(stderr, ERROR_INVALID_CMDLINE);
          return;
        }
        else
        {
          hasin = 1;
        }
      }
    }
    if (output != -1)
    {
      dup2(output, STDOUT_FILENO);
      close(output);
    }
    if (input != -1)
    {
      dup2(input, STDIN_FILENO);
      close(input);
      int w = cmd_count_total_nodes(command_line);
      char *args[w - 3];
      cmd_fetch_command(command_line->next->next->next, args);
      int y = env_list_count_entries(sushenv);
      char *envp[y + 1];
      env_convert_to_list(sushenv, envp);
      execvpe(args[0], args, envp);
      return;
    }
    int w = cmd_count_total_nodes(command_line);
    char *args[w + 1];
    cmd_fetch_command(command_line, args);
    int y = env_list_count_entries(sushenv);
    char *envp[y + 1];
    env_convert_to_list(sushenv, envp);
    execvpe(args[0], args, envp);
    perror("broken");
  }
  else
  {
    int status;
    waitpid(pid, &status, 0);
    return;
  }
}

/**
 * @brief Allows the use to recursively execute commands.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental variables.
 * @param pipesRemaing the number of pipes left in the input.
 */
void execute_recursive(struct list_head *command_line, struct list_head *sushenv, int pipesRemaing)
{
  if (pipesRemaining > 0)
  {
    int status;
    int rc;
    int pipes[2];
    rc = pipe(pipes);
    int pid = fork();
    if (pid == 0)
    {
      if (pipe_in != 0)
      {
        dup2(pipe_in, STDIN_FILENO);
        close(pipe_in);
      }
      dup2(pipes[1], STDOUT_FILENO);
      close(pipes[1]);
      execute_command(command_line, sushenv);
    }
    else
    {
      int new_pipe_amount = pipesRemaining - 1;
      execute_recursive(command_line, sushenv, pipes[0], new_pipe_amount);
      waitpid(pid, &status, 0);
    }
  }
  else
  {
    int status2;
    int pid = fork();
    if (pid == 0)
    {
      if (pipe_in != 0)
      {
        dup2(pipe_in, STDIN_FILENO);
        close(pipe_in);
      }
      execute_command(command_line, sushenv);
    }
    else
    {
      waitpid(pid, &status2, 0);
    }
  }
}

/**
 * @brief Checks to see if something is an internal or external command or if it has pipes.
 * 
 * @param command_line the command line input.
 * @param sushenv the environmental variables.
 */
void execute(struct list_head *command_line, struct list_head *sushenv)
{
  struct cmd *entry = list_entry(command_line->next, struct cmd, list);
  if (entry->state == NULL_TERM)
  {
    return;
  }
  else if (handle_internal_command(command_line, sushenv) != 0)
  {
    return;
  }
  else
  {
    int pipesCount = cmd_count_pipes(command_line);
    if (pipesCount == 0)
    {
      execute_command(command_line, sushenv);
    }
    else
    {
      int outer_status;
      int outer_pid = fork();
      if (outer_pid == 0)
      {
        int status;
        int rc;
        int pipes[2];
        rc = pipe(pipes);
        int command_line_size = get_nodes_in_command(command_line, 0);
        char **command_nodes_array = change_list_to_array(command_line, 0, command_line_size);
        int pid = fork();
        if (pid == 0)
        {
          close(pipes[0]);
          dup2(pipes[1], STDOUT_FILENO);
          int command_line_size = get_nodes_in_command(command_line, 0);
          char **command_nodes_array = change_list_to_array(command_line, 0, command_line_size);
          status = execvp(command_nodes_array[0], command_nodes_array);
        }
        close(pipes[1]);
        dup2(pipes[0], STDIN_FILENO);
        int start_position2 = move_start_position_and_check_pipe(command_line, command_line_size, list_size(command_line), NULL);
        int command_line_size2 = get_nodes_in_command(command_line, start_position2);
        char **command_nodes_array2 = change_list_to_array(command_line, start_position2, command_line_size2);
        execvp(command_nodes_array2[0], command_nodes_array2); //DOESNT WORK
        waitpid(pid, &status, 0);
      }
      else
      {
        waitpid(outer_pid, &outer_status, 0);
      }
    }
  }
}
