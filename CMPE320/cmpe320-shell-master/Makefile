# OBJS=sush.c tokenizer.c list.c executor.c internal.c
# OBJS=sush.c tokenizer/tokenizer.c lists/list.c lists/cmd_list/cmd_list.c lists/env_list/env_list.c execute/executor.c internal_handler/internal.c

OBJS=sush.c tokenizer.c list.c cmd_list.c env_list.c execute.c internal.c

all: sush 

clean:
	rm sush

sush: $(OBJS)
	gcc -D_GNU_SOURCE -g -o sush $(OBJS) 
