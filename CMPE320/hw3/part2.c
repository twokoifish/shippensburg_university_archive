/**
 * @file part2.c
 * @author Andrew Robert Januszko (aj8025@ship.edu)
 * @course CMPE 320 with Dr. Briggs
 * @brief : Part 2 of programming homework 3. Showing how a parent process can wait for a child to complete.
 * @version 0.1
 * @date 2021-03-06
 * 
 * @copyright Copyright (c) 2021
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>

/**
 * @breif: function called by the child process.
 *         allows the child to execute a command.
 */
void child(void)
{
  // the path to the command we want to run.
  char *command = "/bin/ls";

  // the args for the command.
  char *args[] = {"ls", "-l", "/usr/bin", NULL};

  // execute the command.
  execv(command, args);

  // print an error if the execute fails.
  fprintf(stderr, "Error during execution\n");
}

/**
 * @breif: handles the signal output by the child process.
 * 
 * @param signalNumber: the number of the signal.
 */
void signalHandler(int signalNumber)
{
  // print that the child has died and the number of the signal.
  printf("** CHILD DIED %d **\n", signalNumber);
}

/**
 * @breif: function called by the parent process.
 *         allows the partent to wait for and check the status of the child.
 * 
 * @param pid: the process ID of the fork.
 */
void parent(pid_t pid)
{ 
  // handle the death of the child process.
  signal(SIGCHLD, signalHandler);

  int status;

  // wait for the status of the child process.
  waitpid(pid, &status, 0);

  // print the status of the child.
  printf("Child exited: %d\n", status);
}

/**
 * @breif: the main function of the program.
 *         calls fork and then allows each process to call their respective function.
 * 
 * @return int: 0 if all went well.
 */
int main (void)
{
  // fork and store the pid.
  pid_t pid = fork();


  // if the pid is the child, run the child function.
  if (pid == 0) {
    child();

  // else, run the parent function.
  } else {
    parent(pid);
  }

  // return with a status of 0.
  return 0;
}