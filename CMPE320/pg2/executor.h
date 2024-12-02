#ifndef EXECUTER_H
#define EXECUTER_H

#include "internal.h"
#include "tokenizer.h"
#include "env_list.h"
#include "list.h"
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>

void execute_commands(struct list_head *list_nodes, struct list_head sushenvp, int start_node, int recursive_level, int pipe);

#endif