/*
 Programmer: Andrew Januszko      Date of Creation: 03/08/19
 Instructor: Chen Huo             Course: ENGR 120 - 03
 
 A program that calculates two - point form and point - slope form.
*/
#include <stdio.h> /* printf(), scanf() definitions. */
#include <math.h> /* fabs() definition. */

/* Displays the slope - intercept form of the user input. */
void display_slope_intcpt(double m, double b){
  /* Print the title of the function. */
  printf("Slope-intercept form\n");
  /* Holds the absolute value of 'b'. */
  double posB = fabs(b);
  /* If 'b' is greater than or equal to zero. */
  if(b >= 0){
    /* Print the slope - intercept with a minus sign. */
    printf("y = %.2fx - %.2f\n", m, posB);
  /* If 'b' is less than zero. */
  }else{
    /* Print the slope - intercept with a plus sign. */
    printf("y = %.2fx + %.2f\n", m, posB);
  }
}

/* Display the point - slope form of the user input. */
void display_pt_slope(double x1, double y1, double m){
  /* Holds the absolute value of 'x1'. */
  double posX = fabs(x1);
  /* Holds the absolute value of 'y1'. */
  double posY = fabs(y1);
  /* Print the title of the function. */
  printf("\nPoint-slope form\n");
  /* If 'y' is less or equal to zero. */
  if(y1 <= 0){
    /* Print 'y + y1'. */
    printf("y + %.2f", posY);
  /* If 'y' is greater than zero. */
  }else{
    /* Print 'y - y1'. */
    printf("y - %.2f", posY);
  }
  /* Print ' = m'. */
  printf(" = %.2f", m);
  /* If 'x' is less or equal to zero. */
  if(x1 <= 0){
    /* Print 'x + x1'. */
    printf("(x + %.2f)\n\n", posX);
  /* If 'x' is greater than zero. */
  }else{
    /* Print 'x - x1'. */
    printf("(x - %.2f)\n\n", posX);
  }
}

/* Displays the two - point form from the user input. */
void display2_pt(double x1, double y1, double x2, double y2){
  /* Holds the absolute value of 'x1'. */
  double posX1 = fabs(x1);
  /* Holds the absolute value of 'y1'. */
  double posY1 = fabs(y1);
  /* Print the title of the function. */
  printf("\nTwo-point form\n");
  /* If 'y' is less or equal to zero. */
  if(y1 <= 0){
    /* Print 'y2 + y1'. */
    printf("    (%.2f + %.2f)\n", y2, posY1);
  /* If 'y' is greater than zero. */
  }else{
    /* Print 'y2 - y1'. */
    printf("    (%.2f - %.2f)\n", y2, posY1);
  }
  /* Print 'm = ----------------'. */
  printf("m = ----------------\n");
  /* If 'x' is less or equal to zero. */
  if(x1 <= 0){
    /* Print 'x2 + x1'. */
    printf("    (%.2f + %.2f)\n\n", x2, posX1);
  /* If 'x' is greater than zero. */
  }else{
    /* Print 'x2 - x1'. */
    printf("    (%.2f - %.2f)\n\n", x2, posX1);
  }
}

/* Calculates the y_intercept from the point - slope form data provided by the user. */
void intcpt_from_pt_slope(double x1, double y1, double m, double *b){
  /* Assign the result to the pointer 'b'. */
  *b = ((m * x1) - y1);
}

/* Calculates the y_intercept from the two - point form data provided by the user. */
void slope_intcpt_from2_pt(double x1, double y1, double x2, double y2, double *m, double *b){
  /* Assign the result to the pointer 'm'. */
  *m = ((y2 - y1)/(x2 - x1));
  /* Assign the result to the pointer 'b'. */
  *b = ((*m * x1) - y1);
}

/* Get the point - slope form data from the user and assign it to variables. */
void get_pt_slope(double *x1, double *y1, double *m){
  /* Ask the user for coordinates of a point. */
  printf("Enter the x-y coordinates of a point: ");
  /* Store the user input in 'x1' and 'y1'. */
  scanf(" %lf %lf", &*x1, &*y1);
  /* Ask the user for the slope. */
  printf("Enter the slope: ");
  /* Store the user input in 'm'. */
  scanf(" %lf", &*m);
}

/* Get the two - point form data from the user and assign it to variables. */
void get2_pt(double *x1, double *y1, double *x2, double *y2){
  /* Ask the user for the coordinates of the first point. */
  printf("Enter the x-y coordinates for first point: ");
  /* Store the user input in 'x1' and 'y1'. */
  scanf(" %lf %lf", &*x1, &*y1);
  /* Ask the user for the coordinates of the second point. */
  printf("Enter the x-y coordinates for second point: ");
  /* Store the user input in 'x2' and 'y2'. */
  scanf(" %lf %lf", &*x2, &*y2);
}

/* Asks the user what form they would like and checks to see if it exists. */
void get_problem(void){
  /* Holds the form number chosen by then user. */
  int form;
  double x1, /* Holds the first x coordinate entered by the user. */
         y1, /* Holds the first y coordinate entered by the user. */
         x2, /* Holds the second x coordinate entered by the user. */
         y2, /* Holds the second y coordinate entered by the user. */
          m, /* Holds the slope of the line, which may be entered by the user. */
          b; /* Holds the y_intercept of the line. */
  /* Ask the user to pick one of two forms: slope - intercept or two - point. */
  printf("Select the form that you would like "
         "to convert to slope-intercept form:\n"
         "1) Two-point form\n2) Point-slope\n=>");
  /* Store the number of the form that the user chose. */
  scanf(" %d", &form);
  /* Check to see which form the user chose. */
  switch (form) {
    /* If it is the first form, do the calculations for the slope - intercept form. */
    case 1:
      /* Call the 'get2_pt' function and pass in 'x1', 'y1', 'x1', and 'y2'. */
      get2_pt(&x1,&y1,&x2,&y2);
      /* Call the 'slope_intcpt_from2_pt' function and pass in 'x1', 'y1', 'x1', 'y2', 'm', and 'b'. */
      slope_intcpt_from2_pt(x1,y1,x2,y2,&m,&b);
      /* Call the 'display2_pt' function and pass in 'x1', 'y1', 'x1', and 'y2'. */
      display2_pt(x1,y1,x2,y2);
      /* Call the 'display_slope_intcpt' function and pass in 'm' and 'b'. */
      display_slope_intcpt(m,b);
      /* Short cut the function, return to the main function. */
      break;
    /* If it is the second form, do the calculations for the two - point form. */
    case 2:
      /* Call the 'get_pt_slope' function and pass in 'x1', 'y1', and 'm'. */
      get_pt_slope(&x1,&y1,&m);
      /* Call the 'intcpt_from_pt_slope' function and pass in 'x1', 'y1', 'm', and 'b'. */
      intcpt_from_pt_slope(x1,y1,m,&b);
      /* Call the 'display_pt_slope' function and pass in 'x1', 'y1', and 'm'. */
      display_pt_slope(x1,y1,m);
      /* Call the 'display_slope_intcpt' function and pass in 'm' and 'b'. */
      display_slope_intcpt(m,b);
      /* Short cut the function, return to the main function. */
      break;
    /* If it is neither of the listed forms, prompt the user to enter a valid form. */
    default:
      /* Prompt the user to enter a valid form. */
      printf("%d is not a valid form. Please select a valid form from the list below.\n", form);
      /* Call the 'get_problem' function. */
      get_problem();
      /* Short cut the function, return to the main function. */
      break;
  }
}

/* The main function of the program. It calls the 'get_problem' function which starts the program. */
int main(void){
  /* Holds the user input. Predefined with a 'Y' to make the do - while loop run. */
  char userResponse = 'Y';
  /* Do - while the 'userResponse' is equal to 'Y' for 'Yes'. The loop will break if any other character is input. */
  do{
    /* Calls the 'get_problem' function. */
    get_problem();
    /* Ask the user if they would like to do another conversion. */
    printf("Do another conversion (Y or N) => ");
    /* Store the user input into 'userResponse'. */
    scanf(" %c", &userResponse);
  }while(userResponse == 'Y');
  /* Return zero to satify the integer return value of the 'main' function. */
  return 0;
}