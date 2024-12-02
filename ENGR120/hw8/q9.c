/*
  Programmer: Andrew Januszko         Date: 04/11/19
  Instructor: Chen Huo                Course: ENGR 120 - 03

  A program that prints information about the battery.
*/
#include <stdio.h> /* printf() definition. */
#define CHARGE_TIME 900.0 /* The time to charge the battery. */
#define AMPERE_1 4.0 /* The amps of the first device. */
#define AMPERE_2 8.0 /* The amps of the second device. */

/* Holds the information about a battery. */
typedef struct {
  double voltage;
  double capacity;
  double level;
} battery_t;

/* Calculates how much remaining power is in a device. */
void power_device(double ampere, double charge_time, battery_t *battery);
/* Calculates the max time to run the battery on a device. */
double max_time(double ampere, battery_t battery);
/* Recharges the battery. */
void recharge_battery(battery_t *battery);

int main(void) {
  /* Holds the information about a battery. */
  battery_t battery;
  /* Set the max capacity of the battery. */
  battery.capacity = 5000000.0;
  /* Set the current level of the battery. */
  battery.level = battery.capacity;
  /* Set the voltage of the battery. */
  battery.voltage = 12.0;
  /* Calls power_device and lowers the battery level. */
  power_device(AMPERE_1, CHARGE_TIME, &battery);
  /* Print information about the battery. */
  printf("The battery has %.1f joules remaining\n", battery.level);
  /* Print information about the battery. */
  printf("and can power an %.0fA device for %.1f s\n",
   AMPERE_2, max_time(AMPERE_2, battery));
  /* Recharge the battery. */
  recharge_battery(&battery);
  /* Print information about the battery. */
  printf("Recharged, it can power an %.0fA device for %.1f s\n",
   AMPERE_2, max_time(AMPERE_2, battery));
  /* Return 0. */
  return 0;
}

/* Calculates how much remaining power is in a device. */
void power_device(double ampere, double charge_time, battery_t *battery) {
  double watts = ampere * (*battery).voltage;
  double charge = watts * charge_time;
  (*battery).level -= charge;
}

/* Calculates the max time to run the battery on a device. */
double max_time(double ampere, battery_t battery) {
  double watts = ampere * battery.voltage;
  return battery.level / watts;
}

/* Recharges the battery. */
void recharge_battery(battery_t *battery) {
  (*battery).level = (*battery).capacity;
}

  