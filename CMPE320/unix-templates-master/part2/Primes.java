import java.util.*;


class Primes {
    static final int NUM_THRDS = 8;
    static final int CAPACITY = 32;
    static final int MAX_PRIMES = 1000000;

    long to_check[] = new long[CAPACITY];
    int rdPos, wrPos, num, numPrimes;

    boolean isprime(long n)
    {
        if (n == 0) return false;
        if (n == 1) return true;
        if (n == 2) return true;
        if (n == 3) return true;
    
        long i = 2;
        while ((i * i) <= n) {
            if ((n % i) == 0) return false;
            i++;
        }
        return false;
    }

    
    void add_prime(long prime)
    {
        synchronized(to_check) {

            while (num == CAPACITY)  {
                try {
                    to_check.wait();
                } catch(InterruptedException E) { ; }
            }
        

            to_check[wrPos] = prime;
            wrPos++;

            if (wrPos == CAPACITY) {
                wrPos = 0;
            }

            if (num == 0) 
                to_check.notify();

            num++;
        }
    }

    long get_prime()
    {
        long p = 0;

        synchronized(to_check) {

            while(num == 0 ) {
                try {
                    to_check.wait();
                } catch(InterruptedException E) { ; }
            }

        
            p = to_check[rdPos];
            rdPos++;
            if (rdPos == CAPACITY) {
                rdPos = 0;
            }
        
            if (num == CAPACITY) {
                to_check.notify();
            }
        
            num--;
        }
        
        return p;
    }

    void consumer( )
    {
        int done = 0;

        while (done == 0) {

            long p = get_prime();
            if (p == 0) {
                done = 1;
                break;
            }
            if (isprime(p)) 
                numPrimes++;
        }
    
    }

    void producer( )
    {

        long i;
        for (i = 1; i < MAX_PRIMES; i++) {
            add_prime(i);       
        }

        for (i = 0; i < NUM_THRDS; i++)
            add_prime(0);
            
    }



    void run( )
    {
        Thread prodThread;
        Thread consThread[] = new Thread[NUM_THRDS];

        prodThread = new Thread( new  Runnable() { 
            public void run( ) { 
                producer( );
            }
        });
        prodThread.start();

        for (int i = 0; i < NUM_THRDS; i++) {
            consThread[i] = new Thread( new  Runnable() { 
                public void run( ) { 
                    consumer();
                }
            });
            consThread[i].start();
        }

        try {
            prodThread.join();
            for (int i = 0; i < NUM_THRDS; i++) {
                consThread[i].join();
            }
        }
        catch(InterruptedException E)
        {
            
        }

        System.out.println(numPrimes);
    }


    public static void main(String args[]) {
        Primes P = new Primes( );
        P.run( );
    }
}








