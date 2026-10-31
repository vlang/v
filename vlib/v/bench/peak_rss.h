#ifndef V3_BENCH_PEAK_RSS_H
#define V3_BENCH_PEAK_RSS_H

#ifndef V_PEAK_RSS_SCOPE
#define V_PEAK_RSS_SCOPE static inline
#endif

// The result type. A translation unit that also declares the function through a
// 64-bit typedef of its own names that typedef here: `long long` and `int64_t`
// are different types wherever `int64_t` is `long`.
#ifndef V_PEAK_RSS_INT
#define V_PEAK_RSS_INT long long
#endif

#include <sys/resource.h>

V_PEAK_RSS_SCOPE V_PEAK_RSS_INT v3_bench_peak_rss_kb(void) {
	struct rusage usage;
	if (getrusage(RUSAGE_SELF, &usage) != 0) {
		return -1;
	}
#if defined(__APPLE__)
	return (V_PEAK_RSS_INT) usage.ru_maxrss / 1024;
#else
	return (V_PEAK_RSS_INT) usage.ru_maxrss;
#endif
}

#endif
