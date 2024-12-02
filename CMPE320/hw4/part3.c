/**
 * @file part3.c
 * @author Andrew Januszko
 * @brief Calculated the centroid of a set of points.
 * @version 0.1
 * @date 2021-04-07
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#include "part3.h"

/**
 * @brief Get the command line argument for num points, get the start time, call compute, get the end time, print the results, free all of the allocated junk.
 * 
 * @param argc the number of command line arguments.
 * @param argv the command line arguments.
 * @return int 0 if the program executed successfully.
 */
int main(int argc, char **argv)
{
  if (argc != 3)
  {
    fprintf(stderr, "Incorrect number of arguments.\n");
    exit(EXIT_FAILURE);
  } else {
    int num_points = strtod(argv[1], 0);
    int num_threads = strtod(argv[2], 0);
    struct timespec start;
    tick(&start);
    answers_t *answers = compute(num_points, num_threads);
    printf("Centroid: (%.3f,%.3f,%.3f), Average: (%.3f)\n", answers->centroid->x, answers->centroid->y, answers->centroid->z, answers->average_distance);
    printf("It took %ld usecs using %d threads\n", tok(start), num_threads);
    free(answers->centroid);
    free(answers->distances);
    free(answers->points);
    free(answers);
    return 0;
  }
}

/**
 * @brief Initializes a single point using drand48().
 * 
 * @param points the points to create.
 */
void init_point(points_t *points)
{
  points->x = drand48();
  points->y = drand48();
  points->z = drand48();
} // correct

/**
 * @brief Initialize all points.
 * 
 * @param points the points to create.
 * @param num_points the number of points to make.
 */
void init_all_points(points_t *points, int num_points)
{
  for (int i = 0; i < num_points; i++)
  {
    init_point(points);
    points++;
  }
} //correct

/**
 * @brief Create the points array, seed the random number generator, initialize all the points.
 * 
 * @param num_points the number of points to create.
 * @return points_t* the points.
 */
points_t *create_points(int num_points)
{
  srand48(0);
  points_t *points = malloc(num_points * sizeof(points_t));
  if (points == NULL) {
    fprintf(stderr, "Failed to malloc *points\n");
    exit(EXIT_FAILURE);
  }
  init_all_points(points, num_points);
  return points;
} // correct

/**
 * @brief Create the distances array.
 * 
 * @param num_points the number of points.
 * @return double* the distances array.
 */
double *create_distances(int num_points)
{
  size_t points_squared = num_points * num_points;
  double *distances = calloc(points_squared, sizeof(double));
  return distances;
} // correct.

/**
 * @brief Returns the Euclidean distance between two points.
 * 
 * @param a the first point to compare.
 * @param b the second point to compare.
 * @return double the distance between the two points.
 */
double distance(const points_t *a, const points_t *b)
{
  double x = pow(b->x - a->x, 2);
  double y = pow(b->y - a->y, 2);
  double z = pow(b->z - a->z, 2);
  double sum = x + y + z;
  double distance = sqrt(sum);
  return distance;
} // not correct

/**
 * @brief Create the distance array, compute all distances, and return it.
 * 
 * @param points 
 * @param num_points 
 * @return double* 
 */
double *compute_all_distances(const points_t *points, int num_points, int num_threads)
{
  thread_distance_args_t *thread_args[num_threads];
  pthread_t threads[num_threads];
  pthread_attr_t attr[num_threads];

  // break into chunks
  int chunk = (num_points + num_threads - 1) / num_threads;

  for (int i = 0; i < num_threads; i++)
  {
    pthread_attr_init(&attr[i]);
  }

  double *distances = create_distances(num_points);

  for (int i = 0; i < num_threads; i++)
  {
    int start_row = i * chunk;
    int end_row = fmin(start_row + chunk, num_points);
    thread_args[i] = malloc(sizeof(thread_distance_args_t));
    thread_args[i]->start_row = start_row;
    if (i == num_threads - 1) {
      thread_args[i]->end_row = end_row - 1;
    } else {
      thread_args[i]->end_row = end_row;
    }
    thread_args[i]->points = points;
    thread_args[i]->distances = distances;
    thread_args[i]->num_points = num_points;
  }

  for (int i = 0; i < num_threads; i++)
  {
    pthread_create(&threads[i], &attr[i], compute_distance_thread, thread_args[i]);
  }

  for (int i = 0; i < num_threads; i++)
  {
    pthread_join(threads[i], NULL);
  }
  
  return distances;
}

/**
 * @brief Compute the average of all distances in the array.
 * 
 * @param distances the distances of all points.
 * @param num_points the number of points.
 * @return double the average of all distances.
 */
double compute_average(const double *distances, int num_points)
{
  int num_points_squared = pow(num_points, 2);
  double average = 0;
  for (int i = 0; i < num_points_squared; i++) {
    average += distances[i];
  }
  average /= num_points_squared;
  return average;
}

/**
 * @brief Compute the (x, y, z) centroid, allocate a point from the heap, and store the (x, y, z) value in the point and return.
 * 
 * @param points the list of points.
 * @param num_points the number of points.
 * @return points_t* the (x, y, z) centroid.
 */
points_t *compute_centroid(const points_t *points, int num_points)
{
  points_t *point = calloc(1, sizeof(points_t));
  for (int i = 0; i < num_points; i++)
  {
    point->x += points->x;
    point->y += points->y;
    point->z += points->z;
    points++;
  }
  point->x /= num_points;
  point->y /= num_points;
  point->z /= num_points;
  return point;
}

/**
 * @brief Perform all of the above calculations.
 * 
 * @param num_points the number of points.
 * @return answers_t* the answer.
 */
answers_t *compute(int num_points, int num_threads)
{
  answers_t *answers = calloc(1, sizeof(answers_t));
  answers->num_points = num_points;
  answers->points = create_points(num_points);
  answers->distances = compute_all_distances(answers->points, num_points, num_threads);
  answers->average_distance = compute_average(answers->distances, num_points);
  answers->centroid = compute_centroid(answers->points, num_points);
  return answers;
}

/**
 * @brief Save the time of day into start.
 * 
 * @param start holds the time of day.
 */
void tick(struct timespec *start)
{
  clock_gettime(CLOCK_MONOTONIC, start);
}

/**
 * @brief Get the current time and compute the usecs difference between start and now.
 * 
 * @param start the starting time of day.
 * @return long the usecs difference between start and now.
 */
long tok(struct timespec start)
{
  struct timespec end;
  clock_gettime(CLOCK_MONOTONIC, &end);
  long start_us = (start.tv_sec * (long) 1e9) + start.tv_nsec;
  long end_us = (end.tv_sec * (long) 1e9) + end.tv_nsec;
  return ((end_us - start_us) / 1000);
}

/**
 * @brief Threaded version of compute distance.
 * 
 * @param raw_arguments the raw arguments for the thread function.
 * @return void* at the end of the function.
 */
void *compute_distance_thread(void *raw_arguments)
{
  thread_distance_args_t *args = (thread_distance_args_t *)raw_arguments;

  for (int r = args->start_row; r < args->end_row; r++)
  {
    for (int c = 0; c < args->num_points; c++)
    {
      args->distances[r * args->num_points + c] = distance(&args->points[r], &args->points[c]);
    }
  }
}