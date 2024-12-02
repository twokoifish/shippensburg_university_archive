#ifndef PRIME_FACTORIZATION_H
#define PRIME_FACTORIZATION_H

#include "find-key.h"
#include <gmp.h>
#include <stdint.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>

uint64_t modular_pow(uint64_t base, int exponent, uint64_t modulus);
uint64_t prime_factorization(uint64_t public_key, check_key_t *args);
uint64_t gcd(uint64_t a, uint64_t b);
void rho(mpz_t g, mpz_t n);

#endif // PRIME_FACTORIZATION_H
