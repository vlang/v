#ifndef V3_BENCH_PEAK_RSS_H
#define V3_BENCH_PEAK_RSS_H

#ifndef V_PEAK_RSS_SCOPE
#define V_PEAK_RSS_SCOPE static inline
#endif

#include <sys/resource.h>

V_PEAK_RSS_SCOPE long long v3_bench_peak_rss_kb(void) {
	struct rusage usage;
	if (getrusage(RUSAGE_SELF, &usage) != 0) {
		return -1;
	}
#if defined(__APPLE__)
	return (long long) usage.ru_maxrss / 1024;
#else
	return (long long) usage.ru_maxrss;
#endif
}

#endif
