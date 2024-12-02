#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void copy(int *src, int *dest)
{
   int *ptr = dest;
   memset(ptr, 0, 100);
   memcpy(src, dest, 100);
}


int main(int argc, char **argv)
{
   int i;
   int *a = malloc(100);
   int *b = malloc(100);
   int *c = (int *) 0;

  printf("command line args:\n");
  for (i = 0; i < argc; i++) {
    printf("%d %s\n", argc, argv[i]);
  }

  printf("this will be OK\n");
  copy(a,b);
  
  printf("this will crash.\n");
  copy(a,c);
}
