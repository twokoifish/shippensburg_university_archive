/**
 * @file error_msgs.h
 * @author Andrew Januszko & Joshua Burdette 
 * @brief Holds all the error messages for the program.
 * @version 0.1
 * @date 2021-03-31
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef MSGS_H
#define MSGS_H

// if its an error, then
// fprintf(stderr, ERROR_SETENV_ARG, ... )
// 
// if its just a message then
// printf(MSG_STATUS,...)
// 
// whats expected is shown after

#define ERROR_SETENV_ARG "Error - setenv takes two arguments\n"
#define ERROR_UNSETENV_ARG "Error - unsetenv takes one argument\n"
#define ERROR_GETENV_ARG "Error - getenv takes 0 or 1 arguments\n"
#define ERROR_CD_ARG "Error - cd takes one argument\n"
#define ERROR_CD_NOHOME "Error - cd no home directory\n"
#define ERROR_PWD_ARG "Error - pwd takes no arguments\n"
#define ERROR_EXIT_ARG "Rrror - exit takes no arguments\n"
#define ERROR_GETENV_INVALID "Error - getenv unknown variable %s\n" // variable name
#define ERROR_QUEUE_ARG  "Error - queue requires at least two arguments\n"
#define ERROR_OUTPUT_ARG "Error - output takes one argument\n"
#define ERROR_OUTPUT_QUEUED   "Error - task %d is still queued." // task # 0, 1, ...
#define ERROR_OUTPUT_RUNNING "Error - task %d is still running\n" // task # 
#define ERROR_STATUS_ARG "Error - status takes 0 arguments\n"
#define MSG_STATUS_QUEUED "%d - is queued\n" // task #
#define MSG_STATUS_RUNNING "%d is running as pid %d\n" // task #
#define MSG_STATUS_COMPLETE "%d is complete\n" // task #
#define ERROR_CANCEL_ARG "Error - cancel takes one argument\n"
#define MSG_CANCEL_OK "%d is canceled\n" // task #
#define MSG_CANCEL_KILL "%d sending kill signal to pid %d\n" // task #, pid_t
#define ERROR_CANCEL_DONE "%d is already finished, use output %d to show results\n" 
// task #, task #
#define ERROR_EXEC_INFILE "Error - could not open input file : %s\n" 
// strerror(errno)
#define ERROR_EXEC_OUTFILE "Error - could not open output file : %s\n" 
// strerror(errno)
#define ERROR_EXEC_APPEND "Error - could not open output file : %s\n" 
// strerror(errno)
#define ERROR_EXEC_FAILED "Error - could not execute : %s\n" 
// strerror(errno)
#define ERROR_INVALID_CMD "Error could not execute : %s\n" 
// strerror(errno)
#define ERROR_INVALID_CMDLINE "Error - malformed command line.\n"
#endif