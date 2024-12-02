#include <stdlib.h>
#include <stdio.h>
#include "linked_list.h"

int main(void) {
  char ls[] = "HELLO";
  node_t *ll2 = make_list(ls, 5);
  print_list(ll2); // HELLO

  ll2 = insert_at(ll2, 'A', 1); // HAELLO
  print_list(ll2);
  ll2 = insert_at(ll2, 'K', 0); // KHAELLO
  print_list(ll2);
  ll2 = append(ll2, 'O'); // KHAELLOO
  print_list(ll2);
  ll2 = prepend(ll2, 'X'); // XKHAELLOO
  print_list(ll2);

  printf("===removing===\n");

  ll2 = remove_ith(ll2, 0); //KHAELLOO
  print_list(ll2);
  ll2 = remove_ith(ll2, 0); //HAELLOO
  print_list(ll2);
  ll2 = remove_ith(ll2, 2); //HALLOO
  print_list(ll2);
  ll2 = remove_ith(ll2, 4); //HALLO
  print_list(ll2);
  delete_list(&ll2);

  return (0);
}
