/*
  Programmer: Andrew Januszko         Date of Creation:  03/29/2019
  Instructor: Chen Huo                Course: ENGR 120 - 03

  A program that fetches the suffix from two words.
*/
#include <stdio.h> /* printf() definitions. */
#include <string.h> /* strtok(), strcat() definitions. */
#define MAX_SIZE 80 /* The maximum size of a word. */

/* Finds the longest suffix of two words. */
void find_suffix(char word1[MAX_SIZE], char word2[MAX_SIZE]);

int main(void) {
  char word1[MAX_SIZE], /* Holds the first word. */
       word2[MAX_SIZE]; /* Holds the second word. */
  printf("Enter word1: "); /* Prompt for the first word, store it. */
  fgets(word1, sizeof(word1), stdin);
  printf("Enter word2: "); /* Prompt for the second word, store it. */
  fgets(word2, sizeof(word2), stdin);
  find_suffix(word1, word2); /* Fetch the suffix. */
}

/* Finds the longest suffix of two words. */
void find_suffix(char word1[MAX_SIZE], char word2[MAX_SIZE]) {
  strtok(word1, "\n"); /* Remove the new line from the word. */
  strtok(word2, "\n"); /* Remove the new line from the word. */
  size_t length_one = strlen(word1) - 1; /* Get length of the first word. */
  size_t length_two = strlen(word2) - 1; /* Get length of the second word. */
  int max_index = 0; /* Holds the maximum index of the words. */
  /* If the first word is longer than the second word, make it the max index. */
  if(length_one > length_two){
    max_index = length_one;
  /* Otherwise, make the second word the max index. */
  }else{
    max_index = length_two;
  }
  /* Holds the suffix from both words. */
  char suffix[max_index];
  /* If the last letter of word1 and word2 do not match. */
  if(word1[length_one] != word2[length_two]) {
    /* Print a blank line, return nothing. */
    printf("\n");
    return;
  /* If they do match. */
  }else {
    /* Clear the memory in suffix. */
    memset(suffix, '\0', sizeof(&suffix));
    int temp1 = length_one; /* Get length of the first word. */
    int temp2 = length_two; /* Get length of the second word. */
    /* While neither temp length is equal to zero. */
    while(temp1 != 0 && temp2 != 0){
      /* If they match, keep reducing the length. */
      if(word1[temp1] == word2[temp2]){
        temp1--;
        temp2--;
      }
      /* If they dont match, break. */
      else{
        break;
      }
    }
    /* Copy the suffix to the suffix string. */
    strcat(suffix, &word1[temp1 + 1]);
    /* Print the suffix. */
    printf("%s\n", suffix);
  }
}