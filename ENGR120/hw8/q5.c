/*
  Programmer: Andrew Januszko         Date: 04/11/19
  Instructor: Chen Huo                Course: ENGR 120 - 03

  A program that gets IP addresses and states which are on the same network.
*/
#include <stdio.h> /* printf() definitions. */
#include <string.h> /* strcmp() definitions. */
#define MAX_LENGTH 10 /* Max length of input. */
#define MAX_ADDRESSES 100 /* Maximum number of addresses. */
#define SENTINEL "0.0.0.0 none\n" /* The sentinel to stop the loop. */

/* Holds all the parts of an IP address. */
typedef struct {
  int section_one;
  int section_two;
  int section_three;
  int section_four;
  char nickname[MAX_LENGTH];
} address_t;

/* Fetches the addresses from the user until the sentinel is entered. */
int fetch_addresses(address_t addresses[MAX_ADDRESSES]);
/* Checks to see what addresses are on a common network. */
void common_network(address_t addresses[MAX_ADDRESSES], int num_of_addresses);

int main(void) {
  /* Holds all of the addresses entered by the user. */
  address_t addresses[MAX_ADDRESSES];
  /* Holds the number of addresses. */
  int num_of_addresses = fetch_addresses(addresses);
  /* If it is equal to negative one, exit as failure. */
  if(num_of_addresses == -1) {
    printf("Unknown input. Please relaunch the program.\n");
    return 1;
  }
  /* Find the common networks from the addresses. */
  common_network(addresses, num_of_addresses);
  /* Return 0. */
  return 0;
}

/* Fetches the addresses from the user until the sentinel is entered. */
int fetch_addresses(address_t addresses[MAX_ADDRESSES]) {
  /* While less than the maximum number of addresses. */
  for(int i = 0; i < MAX_ADDRESSES; i++) {
    /* Holds the input. */
    char buffer[1024];
    /* Prompt the user for an IP address and name, then store it. */
    printf("Enter IP address and name: ");
    fgets(buffer, sizeof(buffer), stdin);
    /* If the user entered nothing, break. */
    if(strcmp(buffer, "\n") == 0) {
      break;
    }
    /* If the user entered the sentinel phrase, return the current index. */
    if(strcmp(buffer, SENTINEL) == 0) {
      return i;
    }
    /* Else store the address. */
    sscanf(buffer, "%d.%d.%d.%d %[^\n]",
     &addresses[i].section_one,
      &addresses[i].section_two,
       &addresses[i].section_three,
        &addresses[i].section_four,
         addresses[i].nickname);
    /* If at the end of the program, return the current index. */
    if(i == 99) {
      return i;
    }
  }
  /* Return -1 if failed. */
  return -1;
}

/* Checks to see what addresses are on a common network. */
void common_network(address_t addresses[MAX_ADDRESSES], int num_of_addresses) {
  /* Check a value against the other values. */
  for(int i = 0; i < num_of_addresses - 1; i++) {
    for(int j = 1; j < num_of_addresses; j++) {
      /* If the have a matching address and they are not the same address. */
      if((addresses[i].section_one == addresses[j].section_one) && 
      (addresses[i].section_two == addresses[j].section_two) &&
      (i != j)) {
        /* Print the names of the addresses. */
        printf("Machines %s and %s are on the same local network.\n",
         addresses[i].nickname,
          addresses[j].nickname);
      }
    }
  }
}
