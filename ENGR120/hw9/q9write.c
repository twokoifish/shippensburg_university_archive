/*
  Programmer: Andrew Januszko         Date: 04/13/19
  Instructor: Chen Huo                Course: ENGR 120 - 03

  A program that writes IP addresses to a .bin file.
*/
#include <stdio.h> /* printf() definition. */
#include <string.h> /* strcmp() definition. */
#define MAX_LENGTH 10 /* Maximum length of a name. */
#define MAX_ADDRESSES 100 /* Maximum number of addresses. */
#define SENTINEL "0.0.0.0 end\n" /* The stopping input for the loop. */

int main(void) {
  /* Holds the file. */
  FILE *bin = fopen("q9.bin", "wb");
  /* Adds addresses to the file. */
  for(int i = 0; i < MAX_ADDRESSES; i++) {
    char buffer[1024];
    printf("Enter IP address and name: ");
    fgets(buffer, sizeof(buffer), stdin);
    /* If the address is either a new line or the sentinel, exit. */
    if(strcmp(buffer, "\n") == 0 || strcmp(buffer, SENTINEL) == 0) {
      return 0;
    }
    /* Write the address to the file. */
    fprintf(bin, "%s", buffer);
  }
  /* Close the file. */
  fclose(bin);
  /* Return 0. */
  return 0;
}