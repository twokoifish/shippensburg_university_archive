/**
 * @file part1.h
 * @author Andrew Januszko (aj8025)
 * @brief Holds all the functions for part1.
 * @version 0.1
 * @date 2021-04-16
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef PART_1_H
#define PART_1_H

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>
// The maximum number of ping and pongs to have.
#define MAX_SESSIONS 10

/**
 * @brief Holds the shared informatiom between two threads.
 * 
 */
typedef struct
{
  int number;
  sem_t sem;
  pthread_mutex_t lock;
  pthread_cond_t conditional;
} shared_mem_t;

/**
 * @brief Prints "PING: #" and the number, then notifies pong.
 * 
 * @param number the number for ping.
 */
void *ping(void *args);

/**
 * @brief Prints "PONG: #" and the number.
 * 
 * @param number the number for pong.
 */
void *pong(void *args);

#endif /* PART_1_H */