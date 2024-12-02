/*
 * Author: Andrew Januszko
 * Email: aj8025@ship.edu
 * Class: CMPE 320
 * Professor: Dr. Briggs
 * Date of Creation: 02/03/21
 * Date of Completion: 02/05/21
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define MAX_CHAR 4096
#define PERIOD "."

/*
 * Struct to hold data about the sentences.
 * Holds each sentence and count of how many exist.
 */
struct SentenceStructure
{
  int count;
  char **sentences;
};

/* prototype of char *readFromConsole(void). */
void readFromConsole(char *readInput);
/* prototype of int countSentences(char *readInput). */
int countSentences(char *readInput);
/* prototype of void prepareStringArray(struct SentenceStructure *ss). */
void prepareStringArray(struct SentenceStructure *ss);
/* prototype of void breakIntoParts(struct SentenceStructure *ss, char *readInput). */
void breakIntoParts(struct SentenceStructure *ss, char *readInput);
/* prototype of void printAllSentences(struct SentenceStructure ss). */
void printAllSentences(struct SentenceStructure ss);

void freeStringArray(struct SentenceStructure *ss);

/*
 * Main function. Runs all other functions.
 */
int main(void)
{
  /* Allocate memory for the user input then save it. */
  char *readInput = calloc(MAX_CHAR, sizeof(char));

  /* Read the input from the console. */
  readFromConsole(readInput);

  /* Create a struct and then count the sentences. Store the value. */
  struct SentenceStructure ss;
  ss.count = countSentences(readInput);

  /* Print how many sentences were counted. */
  printf("num: %d\n", ss.count);

  /* Prepare the 2D char array. */
  prepareStringArray(&ss);

  /* Break up the big string into smaller ones. */
  breakIntoParts(&ss, readInput);

  free(readInput);

  /* Print each sentence and then exit. */
  printAllSentences(ss);
  
  /* Free the sentences in the struct. */
  freeStringArray(&ss);

  return 0;
}

/*
 * Reads input from the console and returns it.
 */
void readFromConsole(char *readInput)
{
  /* Get input from the console. */
  fgets(readInput, MAX_CHAR, stdin);

  /* Remove the newline charater from the input. */
  readInput = strtok(readInput, "\n");
}

/*
 * Returns the number of sentences from the input.
 */
int countSentences(char *readInput)
{
  int numOfSentences = 0;

  /* If input is empty, return 0. */
  if (readInput == NULL)
  {
    return 0;
  }

  /* Walk through the string. If the current index is a period, increment. */
  for (int index = 0; index < MAX_CHAR; index++)
  {
    if (readInput[index] == PERIOD[0])
    {
      numOfSentences++;
    }
  }
  return numOfSentences;
}

/*
 * Allocates space for the sentences in the SentenceStructure.
 */
void prepareStringArray(struct SentenceStructure *ss)
{
  /* Allocate pointers for the array. */
  ss->sentences = calloc(ss->count, sizeof(char *));
  for (int i = 0; i < ss->count; i++)
  {
    /* Allocate chars for each pointer. */
    ss->sentences[i] = malloc(sizeof(char *) * MAX_CHAR);
  }
}

/*
 * Breaks the input into individual sentences.
 */
void breakIntoParts(struct SentenceStructure *ss, char *readInput)
{
  /* Temp pointer for holding sentences. */
  char *token = strtok(readInput, PERIOD);

  /* Counter for each sentence. */
  int brokenSentences = 0;

  /* Break the input apart. Stop when the end of the string is hit. */
  while (token != NULL && brokenSentences < ss->count)
  {
    if (token[0] == ' ')
    {
      token++;
    }

    /* Copy the token into an index of the array. */
    strcpy(ss->sentences[brokenSentences], token);
    brokenSentences++;
    token = strtok(NULL, PERIOD);
  }
  
}

/*
 * Prints all the sentences in the SentenceStructure.
 */
void printAllSentences(struct SentenceStructure ss)
{
  /* for each sentence we have, print it. */
  for (int i = 0; i < ss.count; i++)
  {
    printf("%d : (%s.)\n", i, ss.sentences[i]);
  }
}

/*
 * Frees the array.
 */
void freeStringArray(struct SentenceStructure *ss)
{
   /* Free the sentences in the struct. */
  for (int i = 0; i < ss->count; i++) {
    free(ss->sentences[i]);
  }
  
  free(ss->sentences);
}