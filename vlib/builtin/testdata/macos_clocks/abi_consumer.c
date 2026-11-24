#include <stdint.h>
#include <time.h>
#include <sys/time.h>

int32_t *v_test_clock_errno(void);

/* External SDK-typed consumer: the production exports and policy are V. */
int v_test_clock_abi(void) {
    struct timespec stamp;
    const struct timespec request = {0, 1};
    *v_test_clock_errno() = 71;
    if (clock_gettime(CLOCK_REALTIME, &stamp) || stamp.tv_sec != 123 ||
        stamp.tv_nsec != 123456789 || *v_test_clock_errno() != 71) return 1;
    if (clock_gettime((clockid_t)999, &stamp) != -1 ||
        *v_test_clock_errno() != 22) return 2;
    *v_test_clock_errno() = 71;
    time_t epoch = 0;
    if (time(&epoch) != 123 || epoch != 123 || *v_test_clock_errno() != 71) return 3;
    if (nanosleep(&request, &stamp) || *v_test_clock_errno() != 71) return 4;
    struct timeval calendar;
    if (gettimeofday(&calendar, 0) || calendar.tv_sec != 123 ||
        calendar.tv_usec != 456789 || *v_test_clock_errno() != 71) return 5;
    const struct timespec invalid = {0, 1000000000};
    if (nanosleep(&invalid, &stamp) != -1 || *v_test_clock_errno() != 22) return 6;
    return 0;
}

int v_test_clock_interrupted(void) {
    const struct timespec request = {1, 4};
    struct timespec remaining;
    if (nanosleep(&request, &remaining) != -1 || *v_test_clock_errno() != 4) return 1;
    if (remaining.tv_sec != 1 || remaining.tv_nsec != 1) return 2;
    return 0;
}
