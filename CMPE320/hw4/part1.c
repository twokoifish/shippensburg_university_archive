/**
 * @file part1.c
 * @author Andrew Januszko
 * @brief Runs n amount of threads each with different sleeps and returns the net time taken.
 * @version 0.1
 * @date 2021-04-05
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "part1.h"

/**
 * @brief Starts the timer, launches the threads, then gets the overall time.
 * 
 * @return int 0 if system exits successfully.
 */
int main(void)
{
  struct timespec start;
  start = clock_start();
  thread_launch();
  uint64_t usElapsed = clock_stop(start);
  clock_time_elapsed(usElapsed);
  return 0;
}

/**
 * @brief Launches the threads.
 * 
 */
void thread_launch(void)
{
  pthread_t threads[MAX_THREADS];
  int thread_param[MAX_THREADS];

  for (int i = 0; i < MAX_THREADS; i++)
  {
    thread_param[i] = (i + 1);
    pthread_create(&threads[i], NULL, sayHello, &thread_param[i]);
  }

  void *thread_return;

  thread_wait(threads, thread_return);
}

/**
 * @brief Waits for the threads to complete.
 * 
 * @param threads the threads.
 * @param thread_return the void * for all the threads to join on.
 */
void thread_wait(pthread_t *threads, void *thread_return)
{
  for (int i = 0; i < MAX_THREADS; i++)
  {
    pthread_join(threads[i], &thread_return);
    
  }
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
 * @brief Says hello after i amount of seconds.
 * 
 * @param ptr the void pointer param for the function.
 * @return void* NULL when the task is complete
 */
void *sayHello(void *ptr)
{
  int i = *(int *) ptr;
  sleep(i);
  printf("Hello from %d seconds in the past\n", i);
  return NULL;
}

/**
 * @brief Get the Elapsed Time object
 * 
 * @param usElapsed the amount of time that elapsed while the process occurred.
 */
void clock_time_elapsed(uint64_t usElapsed)
{
  printf("It took %lu usecs\n", usElapsed);
}
