# Time function tests

A test of the various ways to get time in POSIX and their performance.

 * time => seconds
 * ftime => milliseconds
 * gettimeofday => microseconds
 * clock_gettime => nanoseconds (also supports monotonic clocks)

Linux also implements many of these as a vsyscall or the more modern
vDSO type. These both map a page into the user address space that can
perform readonly system calls like time to avoid the user/kernel
crossing performance hit.

## Results

 * time          (s)  =>   4ns
 * ftime         (ms) =>  39ns
 * gettimeofday  (us) =>  30ns
 * clock_gettime (ns) =>  26ns (CLOCK_REALTIME)
 * clock_gettime (ns) =>   8ns (CLOCK_REALTIME_COARSE)
 * clock_gettime (ns) =>  26ns (CLOCK_MONOTONIC)
 * clock_gettime (ns) =>   9ns (CLOCK_MONOTONIC_COARSE)
 * clock_gettime (ns) => 170ns (CLOCK_PROCESS_CPUTIME_ID)
 * clock_gettime (ns) => 154ns (CLOCK_THREAD_CPUTIME_ID)
 * cached_clock  (s)  =>   0ns

Lower granularity time is the fastest by a fair amount for some
reason. However, cached_clock is still the best, even with the vDSO
stuff...

