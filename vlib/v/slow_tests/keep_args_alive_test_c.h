#include <time.h>

#if defined(_WIN32)
#define __SLEEP_MS(n) Sleep(n)
#elif defined(__APPLE__)
static void __sleep_ms(int ms) {
	struct timespec ts = {
		.tv_sec = ms / 1000,
		.tv_nsec = 1000000L * (ms % 1000)
	};
	struct timespec rem;
	while (nanosleep(&ts, &rem) != 0) {
		ts = rem;
	}
}
#define __SLEEP_MS(n) __sleep_ms(n)
#else
static void __sleep_ms(int ms) {
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	ts.tv_nsec += 1000000L*((ms)%1000);
	ts.tv_sec += ms/1000;
	if (ts.tv_nsec >= 1000000000) {
		ts.tv_nsec -= 1000000000;
		++ts.tv_sec;
	}
	while (clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &ts, NULL) != 0);
}
#define __SLEEP_MS(n) __sleep_ms(n)
#endif

static volatile int** keep;

static int calc_expr_after_delay(int* a, int b, int* c) {
	keep = malloc(1000000);
	keep[43242] = a;
	keep[86343] = c;
	a = NULL;
	c = NULL;
	__SLEEP_MS(200);
	int z = *keep[43242] * b + *keep[86343];
	free(keep);
	return z;
}
