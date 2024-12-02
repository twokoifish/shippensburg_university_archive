#include "brent-factorization.h"

uint64_t prime_factorization(uint64_t n)
{
     if (n % 2 == 0) {
         return 2;
     } else {
         uint64_t y = (rand() % (n - 1)) + 1;
         uint64_t c = (rand() % (n - 1)) + 1;
         uint64_t m = (rand() % (n - 1)) + 1;
         uint64_t g = 1;
         uint64_t r = 1;
         uint64_t q = 1;
         uint64_t ys = 0;
         uint64_t x = 0;
         while (g == 1) {
             x = y;
             for (int i = 0; i < r; i++) {
                 y = ((y * y) % n + c) % n;
             }
             uint64_t k = 0;
             while(k < r && g == 1) {
                ys = y;
                 for (int i = 0; i < (double) fmin(m, r - k); i++) {
                     y = ((y * y) % n + c) % n;
                     uint64_t abs = (x > y) ? x - y : y - x;
                     q = q * abs % n;
                 }
                 g = gcd(q, n);
                 k = k + m;
             }
             r = r * 2;
         }
         if (g == n) {
             while(1) {
                ys = ((ys * ys) % n + c) % n;
                uint64_t abs = (x > ys) ? x - ys : ys - x;
                g = gcd(abs, n);
                if (g > 1) {
                    break;
                }
             }
         }
         return g;
     }


}

uint64_t gcd(uint64_t a, uint64_t b)
{
    while (a != b)
    {
        if (a > b)
        {
            a -= b;
        }
        else
        {
            b -= a;
        }
    }
    return a;
}
