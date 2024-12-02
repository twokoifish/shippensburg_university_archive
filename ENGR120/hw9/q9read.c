/*
  Programmer: Andrew Januszko         Date: 04/13/19
  Instructor: Chen Huo                Course: ENGR 120 - 03

  A program that reads IP addresses from a .bin file.
*/
#include <stdio.h> /* printf() definition. */

int main(void) {
  /* Holds the file. */
  FILE *bin = fopen("q9.bin", "rb");
  char buff1[50], buff2[50];
  /* While the input takes in a while address, print it to terminal. */
  while(fscanf(bin, "%s %s", buff1, buff2) == 2) {
    printf("%s %s\n", buff1, buff2);
  }
  /* Close the file. */
  fclose(bin);
  /* Return 0. */
  return 0;
}