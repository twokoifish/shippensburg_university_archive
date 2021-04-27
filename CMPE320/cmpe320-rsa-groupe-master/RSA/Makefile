all: make-test find-key

CFLAGS=-ggdb -O3

rsa.o: rsa.c rsa.h
main.o: main.c

rsa: rsa.o main.o
	gcc $(CFLAGS) -o rsa $^  -lgmp


make-test: rsa.o make-test.o
	gcc $(CFLAGS) -o make-test $^  -lgmp

find-key: rsa.o find-key.o
	gcc $(CFLAGS) -o find-key $^  -lgmp
	
clean:
	rm -f *.o rsa find-key make-test
