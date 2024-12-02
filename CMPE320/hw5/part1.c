/**
 * @file part1.c
 * @author Andrew Januszko (aj8025)
 * @brief Creates n threads and makes them print ping and pong.
 * @version 0.1
 * @date 2021-04-16
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "part1.h"

/**
 * @brief Creates shared memory for two threads, then creates the two threads.
 * 
 * @return int 0 if all went well, anything else then something when wrong.
 */
int main(void)
{
  // create the shared memory.
  shared_mem_t *mem = calloc(1, sizeof(shared_mem_t));

  // set the starting number to 0.
  mem->number = 0;

  // initialize the conditional variable, throw error if it fails.
  if (pthread_cond_init(&mem->conditional, NULL) > 0)
  {
    perror("Failed to initialize pthread conditional.");
    exit(-1);
  }

  // initialize the mutex lock, throw error if it fails.
  if (pthread_mutex_init(&mem->lock, NULL) > 0)
  {
    perror("Failed to initialize pthread mutex.");
    exit(-1);
  }

  // initialize the semaphore, throw error if it fails.
  if (sem_init(&mem->sem, 0, 0) < 0)
  {
    perror("Failed to allocate semaphore.");
    exit(-1);
  }

  // create the two threads.
  pthread_t ping_thread, pong_thread;

  // start the two threads with the shared memory.
  pthread_create(&ping_thread, NULL, ping, (void *) mem);
  pthread_create(&pong_thread, NULL, pong, (void *) mem);

  // join the two threads on NULL.
  pthread_join(pong_thread, NULL);
  pthread_join(ping_thread, NULL);

  // destroy the semaphore.
  sem_destroy(&mem->sem);

  // free the pointer.
  free(mem);

  return 0;

}

/**
 * @brief Prints "PING: #" and the number, then notifies pong.
 * 
 * @param number the number for ping.
 */
void *ping(void *args)
{
  // convert the arg to shared_mem_t.
  shared_mem_t *mem = (shared_mem_t *)args;

  // while the number is less than the maximum number of pings and pongs.
  while (mem->number < MAX_SESSIONS) {

    // wait for pong to post.
    sem_wait(&mem->sem);

    // lock the mutex.
    pthread_mutex_lock(&mem->lock);

    // increment the number.
    mem->number++;

    // print ping and the number.
    printf("PING: %d\n", mem->number);

    // unlock the mutex.
    pthread_mutex_unlock(&mem->lock);

    // signal to pong.
    pthread_cond_signal(&mem->conditional);
  }

  return NULL;
}

/**
 * @brief Prints "PONG: #" and the number.
 * 
 * @param number the number for pong.
 */
void *pong(void *args)
{
  // convert the arg to shared_mem_t.
  shared_mem_t *mem = (shared_mem_t *)args;

  // while the number is less than the maximum number of pings and pongs.
  while (mem->number < MAX_SESSIONS) {

    // post for ping.
    sem_post(&mem->sem);

    // wait for a signal from ping.
    pthread_cond_wait(&mem->conditional, &mem->lock);

    // print pong and the number.
    printf("PONG: %d\n", mem->number);
  }

  return NULL;
}
