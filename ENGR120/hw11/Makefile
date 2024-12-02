ifeq ($(OPTION), -DQUEUE)

rpn: rpn.o stack.o ll.o stack.h linked_list.h queue.o
	gcc -g -o rpn rpn.o queue.o stack.o ll.o $(OPTION)
else
rpn: rpn.o stack.o ll.o stack.h linked_list.h
	gcc -g -o rpn rpn.o stack.o ll.o $(OPTION)
endif

ifeq ($(OPTION), -DQUEUE)
stack.o: stack.c stack.h
	gcc -g -c queue_stack.c -o stack.o $(OPTION)
else
stack.o: stack.c stack.h
	gcc -g -c stack.c -o stack.o $(OPTION)
endif

queue.o: queue.c queue.h
	gcc -g -c queue.c -o queue.o

rpn.o: rpn.c stack.h
	gcc -g -c rpn.c -o rpn.o $(OPTION)

clean:
	-@rm -f stack.o rpn.o queue.o
	-@rm -f rpn

run: rpn
	./rpn < input
