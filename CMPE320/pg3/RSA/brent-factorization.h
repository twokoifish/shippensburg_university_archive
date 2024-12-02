#ifndef BRENT
#define BRENT

#include "find-key.h"
#include <gmp.h>
#include <stdint.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>

uint64_t brent(uint64_t n);

uint64_t prime_factorization(uint64_t n);

uint64_t gcd(uint64_t a, uint64_t b);


#endif