#include <stdio.h>
#include <stdlib.h>
#include <math.h>


// generate points in the space [0...2]
void generate_point(double *x, double *y)
{
   *x = (drand48()*2.0)-1.0;
   *y = (drand48()*2.0)-1.0; 
}

// compute distance from 0,0
double distance(double x, double y)
{
   double xx = x * x;
   double yy = y * y;
   return sqrt(xx + yy);
} 

int is_incircle(double x, double y)
{
  if (distance(x,y) <= 1.0) return 1;
  else return 0;
}

double estimate_pi(unsigned NUM_POINTS) 
{
    unsigned num_in_circle = 0;
    unsigned i;
    double x,y;

    for (i = 0; i < NUM_POINTS; i++) {
      generate_point(&x, &y);
      num_in_circle += is_incircle(x,y);
    }
 
   return 4.0 * (double) num_in_circle / (double) NUM_POINTS;
} 


int main(int argc, char **argv)
{

   double pi = estimate_pi(1000);
   printf("Pi = %f\n", pi);

}
