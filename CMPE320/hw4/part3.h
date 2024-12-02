/**
 * @file part3.h
 * @author Andrew Januszko
 * @brief Contains all functions for the centroid program.
 * @version 0.1
 * @date 2021-04-07
 * 
 * @copyright Copyright (c) 2021
 * 
 */
#ifndef PART_3_H
#define PART_3_H

#define _GNU_SOURCE 1
#include <stdlib.h>
#include <pthread.h>
#include <stdint.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/time.h>
#include <math.h>

/**
 * @brief holds a point in 3-space (x, y, z).
 * 
 */
typedef struct {
  double x; // the x coordinate.
  double y; // the y coordinate.
  double z; // the z coordinate.
} points_t;

/**
 * @brief Holds the answer for the center of all points.
 * 
 */
typedef struct {
  int num_points; //number of points.
  points_t *points; // an array of points in 3-space (x, y, z).
  double *distances; // a num_points^2 array of the distances between points.
  double average_distance; // the average distance between all points.
  points_t *centroid; // the center of all the points.
} answers_t;

/**
 * @brief Used for passing information into a thread function.
 * 
 */
typedef struct {
  const points_t *points;
  int num_points;
  double *distances;
  int start_row;
  int end_row;
} thread_distance_args_t;

/**
 * @brief Initializes a single point using drand48().
 * 
 * @param points the points to create.
 */
void init_point(points_t *points);

/**
 * @brief Initialize all points.
 * 
 * @param points the points to create.
 * @param num_points the number of points to make.
 */
void init_all_points(points_t *points, int num_points);

/**
 * @brief Create the points array, seed the random number generator, initialize all the points.
 * 
 * @param num_points the number of points to create.
 * @return points_t* the points.
 */
points_t *create_points(int num_points);

/**
 * @brief Create the distances array.
 * 
 * @param num_points the number of points.
 * @return double* the distances array.
 */
double *create_distances(int num_points);

/**
 * @brief Returns the Euclidean distance between two points.
 * 
 * @param a the first point to compare.
 * @param b the second point to compare.
 * @return double the distance between the two points.
 */
double distance(const points_t *a, const points_t *b);

/**
 * @brief Create the distance array, compute all distances, and return it.
 * 
 * @param points 
 * @param num_points 
 * @return double* 
 */
double *compute_all_distances(const points_t *points, int num_points, int num_threads);

/**
 * @brief Compute the average of all distances in the array.
 * 
 * @param distances the distances of all points.
 * @param num_points the number of points.
 * @return double the average of all distances.
 */
double compute_average(const double *distances, int num_points);

/**
 * @brief Compute the (x, y, z) centroid, allocate a point from the heap, and store the (x, y, z) value in the point and return.
 * 
 * @param points the list of points.
 * @param num_points the number of points.
 * @return points_t* the (x, y, z) centroid.
 */
points_t *compute_centroid(const points_t *points, int num_points);

/**
 * @brief Perform all of the above calculations.
 * 
 * @param num_points the number of points.
 * @return answers_t* the answer.
 */
answers_t *compute(int num_points, int num_threads);

/**
 * @brief Save the time of day into start.
 * 
 * @param start holds the time of day.
 */
void tick(struct timespec *start);

/**
 * @brief Get the current time and compute the usecs difference between start and now.
 * 
 * @param start the starting time of day.
 * @return long the usecs difference between start and now.
 */
long tok(struct timespec start);

/**
 * @brief Threaded version of compute distance.
 * 
 * @param raw_arguments the raw arguments for the thread function.
 * @return void* at the end of the function.
 */
void *compute_distance_thread(void *raw_arguments);

#endif /* PART_3_H */