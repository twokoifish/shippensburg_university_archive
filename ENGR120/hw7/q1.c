/*
  Programmer: Andrew Januszko           Date of Creation: 03/27/19
  Instructor: Chen Huo                  Course: ENGR 120 - 03

  A program that sorts a data file in either alphabetical 
  order (ascending) or in numberical order (ascending).
*/
/* printf() definition. */
#include <stdio.h> 
/* atoi() definition. */
#include <stdlib.h>
/* strcpy(), memset(), and strcmp() definitions. */
#include <string.h> 
/* The name of the file that we are reading. */
#define INPUT "data.dat" 
/* The maximum input allowed in the fgets(). */
#define MAX_INTPUT 50 
/* The maximum number of hurricanes allowed in the array. */
#define MAX_HURRICANES 30 
/* The maximum length of data allowed in the array. */
#define MAX_NAME_LENGTH 20  

/*
  Function "print_data" prints the sorted data from the "sort_name"
  and "sort_year" functions.

  It takes in the number of lines in the file, the "year" array,
  "hurricane_name" array, and the "affected_states" array.
*/
void print_data(int lines, 
                int year[], 
                char hurricane_name[MAX_HURRICANES][MAX_NAME_LENGTH], 
                char affected_states[MAX_HURRICANES][MAX_NAME_LENGTH]) {
  /* While "i" is less than the number of lines in the file, print. */
  for(int i = 0; i < lines; i++) {
    /* Prints data. Ex: '1975  Adam    FL LA'. */
    if(i == 0) {
      printf("%-8d%-8s%s\n", year[i], hurricane_name[i], affected_states[i]); 
    }else {
      printf("%-6d%-8s%s\n", year[i], hurricane_name[i], affected_states[i]); 
    }
  }
}

/*
  Function "swap" sorts data passed into it from the "sort_name"
  and "sort_year" functions.

  It takes in the "year" array, "hurricane_name" array, 
  "affected_states" array, and two integers "i" and "j".
*/
void swap(int year[], 
          char hurricane_name[MAX_HURRICANES][MAX_NAME_LENGTH], 
          char affected_states[MAX_HURRICANES][MAX_NAME_LENGTH], 
          int i, 
          int j) {
  /* Holds the initial value of "year[j]". */
  int temp = year[j];
  /* "year[j]" is assigned the value of "year[i]". */
  year[j] = year[i];
   /* "year[i]" is then assigned the value of "temp". */
  year[i] = temp;

  /* Holds the temporary value of "hurricane_name[j]". */
  char dest[MAX_INTPUT];
  /* Copies the initial value of "hurricane_name[j]" into "dest". */
  strcpy(dest, hurricane_name[j]);
  /* Copies the value of "hurricane_name[i]" into "hurricane_name[j]". */
  strcpy(hurricane_name[j], hurricane_name[i]);
  /* Copies the  value of "dest" to "hurricane_name[i]". */
  strcpy(hurricane_name[i], dest);

  /* Copies the initial value of "affected_states[j]" into "dest". */
  strcpy(dest, affected_states[j]);
  /* Copies the value of "affected_states[i]" into "affected_states[j]". */
  strcpy(affected_states[j], affected_states[i]);
  /* Copies the value of "dest" to "affected_states[i]". */
  strcpy(affected_states[i], dest);  
}

/*
  Function "sort_year" sorts data passed into it in 
  numberical order (ascending).

  It takes in the number of lines in the file, the "year" array,
  "hurricane_name" array, and the "affected_states" array.
*/
void sort_year(int lines, 
               int year[], 
               char hurricane_name[MAX_HURRICANES][MAX_NAME_LENGTH], 
               char affected_states[MAX_HURRICANES][MAX_NAME_LENGTH]) {
  /* While "j" is less than "lines - 1", increment. */
  for(int j = 0; j < lines - 1; j++) {
    /* "minimum_index" is assigned the value of "j". */
    int minimum_index = j;
    /* While "i" is less than "lines", increment. */
    for(int i = j + 1; i < lines; i++) {
      /* If "year[i]" is less than "year[minimum_index]". */
      if(year[i] < year[minimum_index]) {
        /* "minimum_index" is assigned the value of "i". */
        minimum_index = i;
      }
    }
    /* If "minimum_index" not equivalent to "j". */
    if(minimum_index != j) {
      /* Call "swap" and swap the values in each array. */
      swap(year, hurricane_name, affected_states, minimum_index, j);
    }
  }
  /* Call "print_data" and print the sorted values from each array. */
  print_data(lines, year, hurricane_name, affected_states);
}

/*
  Function "sort_name" sorts data passed into it in 
  alphabetical order (ascending).

  It takes in the number of lines in the file, the "year" array,
  "hurricane_name" array, and the "affected_states" array.
*/
void sort_name(int lines,
               int year[],
               char hurricane_name[MAX_HURRICANES][MAX_NAME_LENGTH], 
               char affected_states[MAX_HURRICANES][MAX_NAME_LENGTH]) {
  /* While "i" is less than 30, and less than or equal to "lines + 1". */
  for (int i = 0; i < MAX_HURRICANES && i <= lines + 1; i++) {
    /* While "j" less than 30, and less than "lines - 1". */
    for (int j = 0; j < MAX_HURRICANES && j < lines - i - 1; j++) {
      /* If "hurricane_name[j]" is greater than "hurricane_name[j + 1]". */
      if (strcmp(hurricane_name[j], hurricane_name[j + 1]) >= 0) {
        /* Call "swap" and swap the values in each array. */
        swap(year, hurricane_name, affected_states, j + 1, j);
      }
    }
  }
  /* Call "print_data" and print the sorted values from each array. */
  print_data(lines, year, hurricane_name, affected_states);
}

/*
  Function "fetch_data" retrives data from the input file and
  then passes it into the correct sort type.

  It takes in the input file "data", and the int "sortType" chosen 
  by the user.
*/
void fetch_data(FILE *data, int sortType) {
  int character, /* Holds the character at the position in line. */
      number_of_lines = 0, /* Holds the number of lines in the file. */
      i = 0; /* Counter for the fgets() loop. */
  /* Holds the number of years in the file. */
  int year[MAX_HURRICANES];
  /* Holds the maximum amount of input that can be stored. */
  char buffer[MAX_INTPUT];
  /* Holds the number of hurricanes in the file. */
  char hurricane_name[MAX_HURRICANES][MAX_NAME_LENGTH]; 
  /* Holds the number of states in the file. */
  char affected_states[MAX_HURRICANES][MAX_NAME_LENGTH];

  /* While "charater" does not fgetc() End Of File, gather the
     number of lines in a file. */
  do {
    character = fgetc(data);
    if(character == '\n') {
      number_of_lines++;
    }   
  } while (character != EOF);
  if(character != '\n' && number_of_lines != 0) {
    number_of_lines++;
  }
  /* Close the file "data". */
  fclose(data);

  /* Open the file "INPUT" and assign it to "data". */
  data = fopen(INPUT, "r");

  /* While fgets() takes in data, and "i" is less 
     than "number_of_lines", run. */
  while(fgets(buffer, sizeof(buffer), data) && i < number_of_lines) {
    /* Scan the "buffer" into the "years", "hurricane_name", 
       and "affected_states" arrays, and break when it 
       reaches end of line. Otherwise, increment "i". */
    sscanf(buffer, "%d %s %s", &year[i], hurricane_name[i], affected_states[i]);
    if(feof(data)) {
      break;
    }
    i++;
  }
  /* Close the file "data". */
  fclose(data);
  /* Check the integer value of "sortType", and choose the appropriate
     function. */
  switch(sortType) {
    /* If "sortType" is equivalent to 1. */
    case 1:
      /* Call "sort_name", and pass in the "years", "hurricane_name",
         and "affected_states" arrays. */
      sort_name(number_of_lines, year, hurricane_name, affected_states);
      /* Break out of the switch statement. */
      break;
    /* If "sortType" is equivalent to 2. */
    case 2:
      /* Call "sort_year", and pass in the "years", "hurricane_name",
         and "affected_states" arrays. */
      sort_year(number_of_lines, year, hurricane_name, affected_states);
      /* Break out of the switch statement. */
      break;
  }
}

/*
  Function "main" is called by the operating system when the
  user starts the program.

  It takes in no parameters.
*/
int main(void) {
  /* Holds the maximum amount of input that can be stored. */
  char buffer[MAX_INTPUT];
  /* Open the file "INPUT" and assign it to "data". */
  FILE *data = fopen(INPUT, "r");
  /* Create the default user input and assign it to zero. */
  int input = 0;
  /* If the file does not exist. */
  if(data == NULL) {
    /* Print to the console that the file does not exist. */
    fprintf(stderr, "\"%s\" does not exist.\n"
                    "Please verify that the file is in the same"
                    " directory as the program.\n", INPUT);
    /* Close the file "data". */
    fclose(data);
    /* Return a failed exit value. */
    return 1;
  /* If the file does exist. */
  }else {
    /* Print the title of the program, and the avaliable sorting
       options. */
    printf("****** Hurriance Report ********\n"
           "Sort by: 1=Name, 2=year\n");
    /* While "input" is not equal to 1 or 2, run. */
    do {
      /* Print the prompt. */
      printf("Enter choice: ");
      /* Store the user response. */
      char *userResponse = fgets(buffer, sizeof(buffer), stdin);
      /* Take the result and store it into an int. */
      input = atoi(userResponse);
      /* If the response is not equal to 1 or 2, ask for a valid
         sort choice. */
      if(input != 1 && input != 2) {
        /* Print that the input was invalid, and ask for a new input. */
        printf("\nInvalid input!\n"
            "Please select a sort option from the list above.\n");
      }
    }while(input != 1 && input != 2);
    /* Call the "fetch_data" function and pass in the file and the
       user input. */
    fetch_data(data, input);
    /* Close the file "data". */
    fclose(data);
    /* Return a successful exit value. */
    return 0;
  }
}