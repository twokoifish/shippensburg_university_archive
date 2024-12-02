/**
 * @file part2.c
 * @author your name (you@domain.com)
 * @brief 
 * @version 0.1
 * @date 2021-04-06
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "part2.h"

int main(void)
{
  struct timespec start;
  start = clock_start();
  thread_launch(DEFAULT);
  uint64_t usElapsed = clock_stop(start);
  clock_time_elapsed(usElapsed, DEFAULT);
  start = clock_start();
  thread_launch(KERNEL);
  usElapsed = clock_stop(start);
  clock_time_elapsed(usElapsed, KERNEL);
  
}

/**
 * @brief Launches the threads.
 * 
 * @param stack_size the size of the stack. -1 if default.
 */
void thread_launch(int stack_size)
{
  pthread_t thread;
  pthread_attr_t attr;
  pthread_attr_init(&attr);

  if (stack_size != DEFAULT) {
    pthread_attr_setstacksize(&attr, stack_size);
  }

  pthread_create(&thread, &attr, sayStack, NULL);

  void *thread_return;

  thread_wait(thread, thread_return);
}

/**
 * @brief Waits for the threads to complete.
 * 
 * @param threads the threads.
 * @param thread_return the void * for all the threads to join on.
 */
void thread_wait(pthread_t thread, void *thread_return)
{
  pthread_join(thread, &thread_return);
}

/**
 * @brief Get the time that the process started at.
 * 
 * @return struct timespec the time the process started at.
 */
struct timespec clock_start(void)
{
  struct timespec time;
  clock_gettime(CLOCK_MONOTONIC, &time);
  return time;
}

/**
 * @brief Calculate the elapsed time in useconds
 * 
 * @param start the time when the counter started.
 * @return uint64_t the amount of time that elapsed while the process occurred.
 */
uint64_t clock_stop(struct timespec start)
{
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &end);
  uint64_t start_ns = start.tv_sec * (long) 1e9 + start.tv_nsec;
  uint64_t end_ns = end.tv_sec * (long) 1e9 + end.tv_nsec;
  return (end_ns - start_ns) / 1000;
}

/**
 * @brief Says the stack of the thread.
 * 
 * @param ptr the void pointer param for the function.
 * @return void* NULL when the task is complete
 */
void *sayStack(void *ptr)
{
  pthread_attr_t attr;
  pthread_getattr_np(pthread_self(), &attr);
  size_t stack_size;
  pthread_attr_getstacksize(&attr, &stack_size);
  printf("My stack is %ld\n", stack_size);
  return NULL;
}

/**
 * @brief Get the Elapsed Time object
 * 
 * @param usElapsed the amount of time that elapsed while the process occurred.
 * @param stack_size the size of the stack.
 */
void clock_time_elapsed(uint64_t usElapsed, int stack_size)
{
  if (stack_size == DEFAULT) {
     printf("Default thread %ld usecs\n", usElapsed);
  } else {
    printf("Small stack thread %ld usecs\n", usElapsed);
  }
}