#ifndef JBQ_LIST_H
#define JBQ_LIST_H

#include "list.h"
#include <string.h>
#include <limits.h>

enum status {
  complete,
  running,
  waiting
};

struct jbq {
  int id;
  char fileName[PATH_MAX];
  enum status;
  struct list_head list
};

void jbq_list_print(struct list_head *queue);
void jbq_list_clear(struct list_head *queue);
void jbq_list_check(struct list_head *queue);
void jbq_list_fetch(struct list_head *queue, int id);

#endif /* JBQ_LIST_H */