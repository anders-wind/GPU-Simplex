import java.util.function.BiConsumer;

public class Benchmark {

    /** Crude wall clock timing utility, measuring time in microseconds */
    public static class Timer {
      private long start, spent = 0;
      public void pause() { spent += System.nanoTime()-start; } // not reentrant
      public void play() { start = System.nanoTime(); }
      public double check() { return spent/1e3; }
    }

    /**
     * Benchmark a function. (copied from ITU?, slightly modified)
     * the f arg is a two-arg no-return function parameter which takes some argument of
     * type T and a timer. The function should time its critical portion using the timer.
     */
    public static <T> void benchmark(String msg, BiConsumer<T, Timer> f, T arg) {
        int n = 10, count = 1, totalCount = 0;
        double runningTime = 0.0, st = 0.0, sst = 0.0;
        do {
            count *= 2;
            st = sst = 0.0;
            for (int j=0; j<n; j++) {
                Timer t = new Timer();
                for (int i=0; i<count; i++)
                    f.accept(arg, t);
                runningTime = t.check();
                double time = runningTime / count;
                st += time;
                sst += time * time;
                totalCount += count;
            }
        } while (runningTime < 0.25 && count < Integer.MAX_VALUE/2);
        double mean = st/n, sdev = Math.sqrt((sst - mean*mean*n)/(n-1));
        System.err.printf("%-25s mean: %15.1f us, stddev: %10.2f us, count: %10d%n", msg, mean, sdev, totalCount);
    }
}
