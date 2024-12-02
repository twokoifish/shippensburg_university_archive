/**
 * @file part1.h
 * @author Andrew Januszko
 * @brief Header for part1. Contains the prototypes for all of the functions.
 * @version 0.1
 * @date 2021-04-05
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef PART_1_H
#define PART_1_H

#include <stdlib.h>
#include <pthread.h>
#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/time.h>
#define MAX_THREADS 3 // the maximum number of threads made by the program.

/**
 * @brief Launches the threads.
 * 
 */
void thread_launch(void);

/**
 * @brief Waits for the threads to complete.
 * 
 * @param threads the threads.
 * @param thread_return the void * for all the threads to join on.
 */
void thread_wait(pthread_t *threads, void *thread_return);

/**
 * @brief Get the time that the process started at.
 * 
 * @return struct timespec the time the process started at.
 */
struct timespec clock_start(void);

/**
 * @brief Calculate the elapsed time in useconds
 * 
 * @param start the time when the counter started.
 * @return uint64_t the amount of time that elapsed while the process occurred.
 */
uint64_t clock_stop(struct timespec start);

/**
 * @brief Says hello after i amount of seconds.
 * 
 * @param ptr the void pointer param for the function.
 * @return void* NULL when the task is complete
 */
void *sayHello(void *ptr);

/**
 * @brief Get the Elapsed Time object
 * 
 * @param usElapsed the amount of time that elapsed while the process occurred.
 */
void clock_time_elapsed(uint64_t usElapsed);

#endif /* PART_1_H */