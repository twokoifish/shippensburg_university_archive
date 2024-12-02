#include "jbq_list.h"

/**
 * @brief Print all items in the job queue.
 * 
 * @param queue the list of jobs.
 */
void jbq_list_print(struct list_head *queue)
{
  struct list_head *curr = queue->next;
  while (curr != queue)
  {
    struct jbq *entry = list_entry(curr, struct jbq, list);
    printf("%d is %s\n", entry->id, entry->fileName, entry->status);
    curr = curr->next;
  }
}

/**
 * @brief Remove all items from the job queue.
 * 
 * @param queue the list of jobs.
 */
void jbq_list_clear(struct list_head *queue)
{
  while (!list_empty(queue))
  {
    struct jbq *entry = list_entry(queue->next, struct jbq, list);
    list_del(&entry->list);
    free(entry->fileName);
    free(entry);
  }
}

void jbq_list_fetch(struct list_head *queue, int id)
{
}