#include "sieve.h"

#include <gmp.h>

bool* sieve(uint64_t n)
{
    bool *primes = malloc(n * sizeof(bool));
    memset(primes, true, n);

    uint64_t p = 2;
    mpz_t p_check;
    mpz_init_set_ui(p_check, p * p);
    while (mpz_get_ui(p_check) <= n) {

        if(primes[p] == true) {

            mpz_t i;
            mpz_init_set_ui(i, mpz_get_ui(p_check));
            while (mpz_get_ui(i) <= n) {

                primes[mpz_get_ui(i)] = false;
                mpz_add_ui(i, i, p);
            }
        }
        p++;
    }

    return primes;
}

int main() {
    uint64_t n = UINT64_MAX;
    bool* set = sieve(n);
    for (uint64_t i = 0; i < UINT64_MAX; i++) {
        if (set[i] == true) {
            printf("%llu\n", i);
        }
    }
}