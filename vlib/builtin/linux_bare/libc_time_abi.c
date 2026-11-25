#include "libc_compat.h"
#include "libc_time_abi.h"
#include <stdlib.h>
#include <string.h>

/* Native struct tm field layout and typedefs are confined to this adapter. */
static struct tm v_time_calendar[2];
static char v_time_text[2][26];
long timezone;
int daylight;
char *tzname[2] = {"UTC", "UTC"};
#if defined(__GLIBC__)
extern __typeof__(timezone) __timezone __attribute__((alias("timezone")));
extern __typeof__(daylight) __daylight __attribute__((alias("daylight")));
extern __typeof__(tzname) __tzname __attribute__((alias("tzname")));
#endif
uintptr_t v_time_tm_static(int32_t kind) { return (uintptr_t)&v_time_calendar[kind]; }
uintptr_t v_time_text_static(int32_t kind) { return (uintptr_t)v_time_text[kind]; }
void v_time_tm_set(uintptr_t address, uintptr_t fields, int64_t offset, uintptr_t zone) {
    struct tm *value = (struct tm *)address;
    const int32_t *f = (const int32_t *)fields;
    memset(value, 0, sizeof *value);
    value->tm_year=f[0]; value->tm_mon=f[1]; value->tm_mday=f[2];
    value->tm_hour=f[3]; value->tm_min=f[4]; value->tm_sec=f[5];
    value->tm_wday=f[6]; value->tm_yday=f[7]; value->tm_isdst=f[8];
    value->tm_gmtoff=offset; value->tm_zone=(const char *)zone;
}
void v_time_tm_get(uintptr_t address, uintptr_t fields) {
    const struct tm *value = (const struct tm *)address;
    int32_t *f = (int32_t *)fields;
    f[0]=value->tm_year; f[1]=value->tm_mon; f[2]=value->tm_mday;
    f[3]=value->tm_hour; f[4]=value->tm_min; f[5]=value->tm_sec;
    f[6]=value->tm_wday; f[7]=value->tm_yday; f[8]=value->tm_isdst;
}
void v_time_names_set(int64_t offset, int32_t dst, uintptr_t standard, uintptr_t summer) {
    timezone=(long)offset; daylight=dst; tzname[0]=(char *)standard; tzname[1]=(char *)summer;
}
int32_t v_time_errno_get(void) { return errno; }
void v_time_errno_set(int32_t value) { errno=value; }
uintptr_t v_time_clocks_per_second(void) { return CLOCKS_PER_SEC; }
extern int64_t v_time_now(uintptr_t);
extern uintptr_t v_time_gmtime(uintptr_t, uintptr_t);
extern uintptr_t v_time_localtime(uintptr_t, uintptr_t);
extern uintptr_t v_time_localtime_default(uintptr_t, uintptr_t);
extern uintptr_t v_time_ctime_default(uintptr_t, uintptr_t);
extern int64_t v_time_mktime(uintptr_t, int32_t);
extern void v_time_tzset(void);
extern uintptr_t v_time_asctime(uintptr_t, uintptr_t);
extern uintptr_t v_time_ctime(uintptr_t, uintptr_t);
extern int64_t v_time_clock(void);
extern int32_t v_time_clock_getres(int32_t, uintptr_t);
extern double v_time_difftime(int64_t, int64_t);
time_t time(time_t *output) { return (time_t)v_time_now((uintptr_t)output); }
struct tm *gmtime_r(const time_t *stamp, struct tm *output) { return (struct tm *)v_time_gmtime((uintptr_t)stamp, (uintptr_t)output); }
struct tm *gmtime(const time_t *stamp) { return (struct tm *)v_time_gmtime((uintptr_t)stamp, v_time_tm_static(0)); }
struct tm *localtime_r(const time_t *stamp, struct tm *output) { return (struct tm *)v_time_localtime((uintptr_t)stamp, (uintptr_t)output); }
struct tm *localtime(const time_t *stamp) { return (struct tm *)v_time_localtime_default((uintptr_t)stamp, v_time_tm_static(1)); }
time_t timegm(struct tm *value) { return (time_t)v_time_mktime((uintptr_t)value, 0); }
time_t mktime(struct tm *value) { return (time_t)v_time_mktime((uintptr_t)value, 1); }
void tzset(void) { v_time_tzset(); }
char *asctime_r(const struct tm *value, char *buffer) { return (char *)v_time_asctime((uintptr_t)value, (uintptr_t)buffer); }
char *asctime(const struct tm *value) { return (char *)v_time_asctime((uintptr_t)value, v_time_text_static(0)); }
char *ctime_r(const time_t *stamp, char *buffer) { return (char *)v_time_ctime((uintptr_t)stamp, (uintptr_t)buffer); }
char *ctime(const time_t *stamp) { return (char *)v_time_ctime_default((uintptr_t)stamp, v_time_text_static(1)); }
clock_t clock(void) { return (clock_t)v_time_clock(); }
int clock_getres(clockid_t clock, struct timespec *result) { return v_time_clock_getres(clock, (uintptr_t)result); }
double difftime(time_t left, time_t right) { return v_time_difftime(left, right); }
