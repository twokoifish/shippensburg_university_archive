all: make-test find-key

CFLAGS=-ggdb -O3

rsa.o: rsa.c rsa.h
main.o: main.c
prime_factorization.o: prime_factorization.c prime_factorization.h
brent-factorization.o: brent-factorization.c brent-factorization.h

brent-factorization: brent_factorization.o
	gcc $(CFLAGS) -o brent $^ -lgmp 

prime_factorization: prime_factorization.o
	gcc $(CFLAGS) -o prime $^ -lgmp

rsa: rsa.o main.o
	gcc $(CFLAGS) -o rsa $^  -lgmp

make-test: rsa.o make-test.o
	gcc $(CFLAGS) -o make-test $^  -lgmp

find-key: rsa.o prime_factorization.o find-key.o
	gcc $(CFLAGS) -o find-key $^ -lpthread -lgmp
	
clean:
	rm -f *.o rsa find-key make-test
