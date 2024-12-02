/*
  Programmer: Andrew Januszko         Date of Creation:  03/29/2019
  Instructor: Chen Huo                Course: ENGR 120 - 03

  A program that reverses the order of a string provided by the user.
*/
#include <stdio.h> /* printf() definitions. */
#include <string.h> /* strcpy(), strtok(), strcat() definitions. */
#include <ctype.h> /* isspace() definitions. */
#define MAX_WORDS 16 /* The maximum amount of words. */
#define MAX_SIZE 80 /* The maximum size of a word. */
#define MAX_SENTENCE 1295 /* The maximum size of a string. */

/* Reverses a string then returns it. */
char *reverse_sentence(char sentence[MAX_SENTENCE]);

int main(void) {
  /* Holds the user input. */
  char sentence[MAX_SENTENCE];
  /* Prints the prompt for input. */
  printf("Input: ");
  /* Fetch the user input. */
  fgets(sentence, sizeof(sentence), stdin);
  /* Prints the output from the function. */
  printf("Output: %s", reverse_sentence(sentence));
}

/* Reverses a string then returns it. */
char *reverse_sentence(char sentence[MAX_SENTENCE]) {
  /* Holds the length of the sentence. */
  size_t length = strlen(sentence);
  int whitespace = 0, /* Holds the amount of whitespace. */
      index = 0; /* Holds the index of the string. */
  char *token; /* The token from strtok(). */
  const char delim[2] = " "; /* Holds the delimeter. */
  /* Remove the new line from the string. */
  strtok(sentence, "\n");
  /* If the current index is whitespace, increment whitespace. */
  for(int i = 0; i <= length; i++) {
    if(isspace(sentence[i])){
      whitespace++;
    }
  }
  /* Set the index equal to whitespace. */
  index = whitespace;
  /* Holds the individual words from the sentence. */
  char holder[whitespace][MAX_SENTENCE];
  /* Take the first word off of the sentence. */
  token = strtok(sentence, delim);

  /* While the token is not equal to NULL. */
  while(token != NULL) {
    /* Copy the string to the last index of the holder. */
    strcpy(holder[index], token);
    /* Take the first word off of the sentence. */
    token = strtok(NULL, delim);
    /* Reduce. */
    index--;
  }

  /* Clear the sentence. */
  memset(sentence, '\0', sizeof(&sentence));

  /* While i is less than or equal to whitespace. */
  for(int i = 0; i <= whitespace; i++) {
    /* Concatenate the holder onto the sentence. */
    strcat(sentence, holder[i]);
    /* Concatenate a space onto the sentence. */
    strcat(sentence, delim);
    /* If the current index is equal to whitespace. Add a new line. */
    if(i == whitespace) { 
      strcat(sentence, "\n");
    }
  }
  /* Return the sentence. */
  return sentence;
}