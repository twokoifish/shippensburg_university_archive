public class jprimes {

    static boolean isprime(long n)
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
        return true;

    }


    private class Work {
        long start;
        long end;
        long count;
        long start_time;
        long nanos;
    }


    private class PrimesRunner implements Runnable {

        Work workunit;
        public PrimesRunner(Work workunit)
        {
            this.workunit = workunit;
        }

        public void run( )
        {
            workunit.start_time = System.nanoTime();

            workunit.count= 0;
            for (long i = workunit.start; i < workunit.end; i++) {
                if (isprime(i))
                    workunit.count++;
            }

            workunit.nanos = System.nanoTime() - workunit.start_time;
        }

    }

    public void fmain( )
    {
        final long target = 1000000;
        final int num_procs = 512;

        long start = System.nanoTime();


        Work workunits[] = new Work[num_procs];
        PrimesRunner runners[] = new PrimesRunner[num_procs];
        Thread thrds[] = new Thread[num_procs];

        for (int i = 0; i < num_procs; i++) {
            workunits[i] = new Work( );
            workunits[i].start = target / num_procs * i;
            workunits[i].end = workunits[i].start + target / num_procs;

            runners[i] = new PrimesRunner(workunits[i]);

            thrds[i] = new Thread( runners[i] );
            thrds[i].start();
        }

        long total_count = 0;
        for (int i = 0; i < num_procs; i++) {
            
            try {
                thrds[i].join();
            }
            catch(InterruptedException E)
            {
                E.printStackTrace();;
            }

            total_count += workunits[i].count;
            System.out.format("took %d ns\n", workunits[i].nanos);

        }


        System.out.format("total count %d\n", total_count);

        long end = System.nanoTime();
        System.out.format("%d ns\n", (end-start));
    }

    public static void main(String args[])
    {
        jprimes J = new jprimes( );
        J.fmain();
    }
}
