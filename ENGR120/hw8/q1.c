/*
  Programmer: Andrew Januszko         Date: 04/09/19
  Instructor: Chen Huo                Course: ENGR 120 - 03

  A program that takes vehicle data from the user and presents
  it in a more organized format.
*/
#include <stdio.h> /* printf() definitions. */
#include <string.h> /* strlen() definitions. */
#include <ctype.h> /* isspace() definitions. */
#define INPUT_LENGHT 1024 /* Maximum size of the buffer. */

/* Holds the compiled date format. */
typedef struct {
  char compiled[INPUT_LENGHT];
} date_t;

/* Holds the capacity and current level of the tank. */
typedef struct {
  double capacity;
  double level;
} tank_t;

/* Holds the make, model, odometer, manufacter date,
   purchase date, and tank data. */
typedef struct {
  char make[INPUT_LENGHT];
  char model[INPUT_LENGHT];
  int odometer;
  date_t manufacture_date;
  date_t purchase_date;
  tank_t tank;
} vehicle_t;

/* Scans in the vehicle information. */
int scan_vehicle(vehicle_t *vehicle, char buffer[INPUT_LENGHT]);
/* Scans in the manufacture and purchase dates. */
void scan_date(vehicle_t *vehicle, char buffer[INPUT_LENGHT]);
/* Scans in the tank data. */
void scan_tank(vehicle_t *vehicle, char buffer[INPUT_LENGHT]);
/* Reformats the manufacture and purchase dates. */
char *format_input(char buffer[INPUT_LENGHT]);
/* Prints the vehicle information. */
void print_vehicle(vehicle_t vehicle);
/* Prints the manufacture and purchase dates. */
void print_date(vehicle_t vehicle);
/* Prints the tank data. */
void print_tank(vehicle_t vehicle);

int main(void) {
  /* Makes a new vechicle object. */
  vehicle_t vehicle;
  /* Holds the user input. */
  char buffer[INPUT_LENGHT];
  /* Runs until the user breaks it. */
  while(1) {
    /* If 'scan_vehicle' returns 1, exit as failure. */
    if(scan_vehicle(&vehicle, buffer) == 1) { 
      return 1;
    }
    /* Calls scan_date and passes in vehicle and buffer. */
    scan_date(&vehicle, buffer);
    /* Calls scan_tank and passes in vehicle and buffer. */
    scan_tank(&vehicle, buffer);
    /* Calls print_vehicle and passes in vehicle. */
    print_vehicle(vehicle);
    /* Calls print_date and passes in vehicle. */
    print_date(vehicle);
    /* Calls print_tank and passes in vehicle. */
    print_tank(vehicle);
    /* Print new line. */
    printf("\n");
  }
  /* Exit as success. */
  return 0;
}

/* Get the information about the vehicle. */
int scan_vehicle(vehicle_t *vehicle, char buffer[INPUT_LENGHT]) {
  /* Prompt the user for the vehicle make. */
  printf("Enter make: ");
  /* If the buffer is equal to NULL, exit as failure. */
  while(fgets(buffer, INPUT_LENGHT, stdin) == NULL) {
    return 1;
  }
  /* Store the vehicle make. */
  sscanf(buffer, "%s", (*vehicle).make);
  /* Prompt the user for the model, and fgets it. */
  printf("Enter model: ");
  fgets(buffer, INPUT_LENGHT, stdin);
  /* Store the vehicle model. */
  sscanf(buffer, "%s", (*vehicle).model);
  /* Prompt the user for the odometer, and fgets it. */
  printf("Enter odometer: ");
  fgets(buffer, INPUT_LENGHT, stdin);
  /* Store the vehicle odometer. */
  sscanf(buffer, "%d", &(*vehicle).odometer);
  /* Exit as success. */
  return 0;
}

void scan_date(vehicle_t *vehicle, char buffer[INPUT_LENGHT]) {
  /* Prompt the user for the manufacture date, and fgets it. */
  printf("Enter manufacture date (Y M D): ");
  fgets(buffer, INPUT_LENGHT, stdin);
  /* Change the date to a more readable format. */
  format_input(buffer);
  /* Store the compiled date. */
  sscanf(buffer, "%[^\n]", (*vehicle).manufacture_date.compiled);
  /* Prompt the user for the purchase date, and fgets it. */
  printf("Enter purchase date (Y M D): ");
  fgets(buffer, INPUT_LENGHT, stdin);
  /* Change the date to a more readable format. */
  format_input(buffer);
  /* Store the compiled date. */
  sscanf(buffer, "%[^\n]", (*vehicle).purchase_date.compiled);
}

/* Calls scan_tank and passes in vehicle and buffer. */
void scan_tank(vehicle_t *vehicle, char buffer[INPUT_LENGHT]) {
  /* Prompt the user for the tank capacity and level, and fgets it. */
  printf("Enter tank capacity and level: ");
  fgets(buffer, INPUT_LENGHT, stdin);
  /* Store the tank capacity and level. */
  sscanf(buffer, "%lf %lf", &(*vehicle).tank.capacity, &(*vehicle).tank.level);
}

/* Reformats the dates to the proper format. */
char *format_input(char buffer[INPUT_LENGHT]) {
      /* Holds the starting index. */
  int index = 1,
      /* Holds the current character. */
      current_character,
      /* Holds the size of the string, minus the new line. */
      size = strlen(buffer) - 1;
  /* While in the index of the sentence. */
  while(buffer[index]) {
    /* Mark the current character equal to the current character. */
    current_character = buffer[index];
    /* If the current index is equal to the size of the sentence. */
    if(index == size) {
      break;
    }else{
      /* If the current character is equal to a space. */
      if(isspace(current_character)) {
        /* Set the space equal to a slash. */
        current_character = '/';
      }
    }
    /* Set the character at the index equal to the current character. */
    buffer[index] = current_character;
    /* Increment the index. */
    index++;
  }
  /* Return the buffer. */
  return buffer;
}

/* Print the vehicle information. */
void print_vehicle(vehicle_t vehicle) {
  printf("Make: %s\n", vehicle.make);
  printf("Model: %s\n", vehicle.model);
  printf("Odometer: %d\n", vehicle.odometer);
}

/* Print the manufacture and purchase dates. */
void print_date(vehicle_t vehicle) {
  printf("Manufacture date: %s\n", vehicle.manufacture_date.compiled);
  printf("Purchase date: %s\n", vehicle.purchase_date.compiled);
}

/* Prink the tank capacity, level, and percentage. */
void print_tank(vehicle_t vehicle) {
  double percentage = (vehicle.tank.level/vehicle.tank.capacity) * 100;
  printf("Tank: %.1f/%.1f (%.1f%%)\n",
         vehicle.tank.level, 
         vehicle.tank.capacity, 
         percentage);
}