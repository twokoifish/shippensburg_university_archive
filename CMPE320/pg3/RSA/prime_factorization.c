#include "prime_factorization.h"

uint64_t modular_pow(uint64_t base, int exponent, uint64_t modulus)
{
    uint64_t result = 1;
    while (exponent > 0)
    {
        if (exponent & 1)
        {
            result = (result * base) % modulus;
        }
        exponent = exponent >> 1;
        base = (base * base) % modulus;
    }
    return result;
}

uint64_t prime_factorization(uint64_t public_key, check_key_t *args)
{
    srand(time(NULL));
    if (public_key == 1)
    {
        args->foundKey = true;
        return public_key;
    }
    uint64_t x = (rand() % (public_key - 3)) + 3;
    uint64_t y = x;
    uint64_t c = (rand() % (public_key - 1)) + 1;
    uint64_t d = 1;
    while (d == 1)
    {
        x = (modular_pow(x, 2, public_key) + c + public_key) % public_key;
        y = (modular_pow(y, 2, public_key) + c + public_key) % public_key;
        y = (modular_pow(y, 2, public_key) + c + public_key) % public_key;
        uint64_t abs = (x > y) ? x - y : y - x;
        uint64_t pkcopy = public_key;
        while (abs != pkcopy)
        {
            if (abs > pkcopy)
            {
                abs -= pkcopy;
            }
            else
            {
                pkcopy -= abs;
            }
        }
        d = abs;
        if (d == public_key)
        {
            return prime_factorization(public_key, args);
        }
    }
    args->foundKey = true;
    return d;
}

void f(mpz_t result, mpz_t base, mpz_t modulus)
{
    int exponent = 2;

    mpz_t f_base;
    mpz_init(f_base);

    mpz_t f_modulus;
    mpz_init(f_modulus);


    mpz_t f_result;
    mpz_init_set_ui(f_result, 1);

    while (exponent > 0)
    {
        if (exponent & 1)
        {
            mpz_t temp;
            mpz_init(temp);
            mpz_mul(temp, f_result, f_base);
            mpz_mod(f_result, temp, f_modulus);    
        }
        exponent = exponent >> 1;
        mpz_t temp;
        mpz_init(temp);
        mpz_mul(temp, f_base, f_base);
        mpz_mod(f_base, temp, f_modulus);    
    }

    mpz_set(result, f_result);
}

//Pollard rho factoring algorithm
void rho(mpz_t g, mpz_t n)
{
    mpz_t x;
    mpz_t y;
    mpz_init_set_ui(x ,2);
    mpz_init_set_ui(y ,2);//initialize x and y as 2
    mpz_set_ui(g , 1);
    mpz_t temp;
    mpz_init(temp);

    if(mpz_probab_prime_p(n,25) != 0)
        return;//test if n is prime with miller rabin test

    int count;
    int t1 = 0;
    int t2 = 1;
    int nextTerm = t1 + t2;
    while(mpz_cmp_ui(g,1) < 1)
    {
        f(x, x, n);//x is changed
        f(y, y, n);//y is going through the sequence twice as fast
        f(y, y, n);

        if(count == nextTerm)//calculate gcd every fibonacci number
        {
            mpz_sub(temp,x,y);
            mpz_gcd(g , temp, n);

            t1 = t2;
            t2 = nextTerm;
            nextTerm = t1 + t2;//calculate next fibonacci number
        }

        count ++;
    }

    return;
}