main_ll.o: main_ll.c linked_list.h
	gcc -c main_ll.c

ll.o: ll.c linked_list.h
	gcc -c ll.c

main_ll: main_ll.o ll.o
	gcc -o main_ll main_ll.o ll.o

al.o: al.c array_list.h
	gcc -c al.c

main_al.o: main_al.c array_list.h
	gcc -c main_al.c

main_al: main_al.o al.o
	gcc -o main_al main_al.o al.o

runll: main_ll
	./main_ll

runal: main_al
	./main_al

both: main_ll main_al

runboth: both
	./main_ll
	./main_al

clean:
	-@rm -f *.o main_ll main_al
