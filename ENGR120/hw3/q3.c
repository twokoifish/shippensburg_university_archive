/*
 Programmer: Andrew Januszko        Date of Creation: 02/20/19
 Instructor: Chen Huo               Course: ENGR 120 - 03
 
 An example of short circuiting in C.
*/
#include <stdio.h> /* printf, scanf definitions. */

/* Definition for 'fun1' function. */
int fun1(void);

/* Definition for 'fun2' function. */
int fun2(void);

int main(void){
  /* Run the first test. */
  printf("Testing &&\n");
    /* If true. */
    if (fun1() == 1 && fun2() == 1) {
        /* Complete && and continue to ||. */
        printf("Test of && complete\n");
        printf("Testing ||\n");
        /* If true. */
        if (fun1() == 1) {
            /* Complete ||. */
            printf("Test of || complete\n");
        /* If false. */
        } else {
            /* Call 'fun2()'. */
            fun2();
            /* Complete ||. */
            printf("Test of || complete\n");
        }
    /* If false. */
    }else{
        /* Run the second test. */
        printf("Testing ||\n");
        /* If true. */
        if (fun1() == 1) {
            /* Complete ||. */
            printf("Test of || complete\n");
        /* If false. */
        } else {
            /* Call 'fun2()'. */
            fun2();
            /* Complete ||. */
            printf("Test of || complete\n");
        }
    }
    /* Return 0. */
    return 0;
}

/* Decide what to return based on user input. */
int fun1(void){
    char temp; /* Holds the user input. */
    
    /* User either enters T or F, this gets stored. */
    printf("Enter T or F: ");
    scanf("%c%*c", &temp);
    
    /* Test the input to see if it returns a 1 or 0. */
    if (temp == 'T') {
        return 1;
    } else {
        return 0;
    }
}

/* Print a statement and always return 1. */
int fun2(void){
    printf("fun2 executed\n");
    return 1;
}
