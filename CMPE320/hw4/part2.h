/**
 * @file part2.h
 * @author your name (you@domain.com)
 * @brief 
 * @version 0.1
 * @date 2021-04-06
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef PART_2_H
#define PART_2_H

#define DEFAULT -1
#define KERNEL 131072

#define _GNU_SOURCE 1
#include <stdlib.h>
#include <pthread.h>
#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/time.h>

/**
 * @brief Launches the threads.
 * 
 * @param stack_size the size of the stack. -1 if default.
 */
void thread_launch(int stack_size);

/**
 * @brief Waits for the threads to complete.
 * 
 * @param thread the thread.
 * @param thread_return the void * for all the threads to join on.
 */
void thread_wait(pthread_t thread, void *thread_return);

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
 * @brief Says the stack of the thread.
 * 
 * @param ptr the void pointer param for the function.
 * @return void* NULL when the task is complete
 */
void *sayStack(void *ptr);

/**
 * @brief Get the Elapsed Time object
 * 
 * @param usElapsed the amount of time that elapsed while the process occurred.
 * @param stack_size the size of the stack.
 */
void clock_time_elapsed(uint64_t usElapsed, int stack_size);


#endif /* PART_2_H */