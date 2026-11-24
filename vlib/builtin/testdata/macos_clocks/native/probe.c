#include <stdint.h>
#include <time.h>
#include <sys/time.h>
#include <errno.h>
#include <sys/syscall.h>
#define CHECK(value, code) do {if(!(value))return(code);}while(0)
int v_macos_clock_abi_probe(void) {
 static const clockid_t clocks[]={CLOCK_REALTIME,CLOCK_MONOTONIC,CLOCK_MONOTONIC_RAW,
  CLOCK_MONOTONIC_RAW_APPROX,CLOCK_UPTIME_RAW,CLOCK_UPTIME_RAW_APPROX,
  CLOCK_PROCESS_CPUTIME_ID,CLOCK_THREAD_CPUTIME_ID};
 for(unsigned i=0;i<sizeof clocks/sizeof clocks[0];i++) {
  struct timespec stamp;errno=77;
  CHECK(clock_gettime(clocks[i],&stamp)==0 && errno==77,10+(int)i);
  CHECK(stamp.tv_sec>=0 && stamp.tv_nsec>=0 && stamp.tv_nsec<1000000000,20+(int)i);
 }
 struct timespec stamp;
 CHECK(clock_gettime((clockid_t)-42,&stamp)==-1 && errno==EINVAL,30);
 errno=77;time_t copied=0;time_t now=time(&copied);
 CHECK(now==copied && now>1700000000 && errno==77,31);
 struct timeval wall;
 CHECK(gettimeofday(&wall,0)==0 && errno==77 && wall.tv_sec>=now-1 && wall.tv_sec<=now+1,32);
 CHECK(wall.tv_usec>=0 && wall.tv_usec<1000000,33);
 struct timespec invalid;invalid.tv_sec=0;invalid.tv_nsec=1000000000;
 CHECK(nanosleep(&invalid,0)==-1 && errno==EINVAL,35);
 struct timespec zero,remaining;zero.tv_sec=0;zero.tv_nsec=0;remaining.tv_sec=7;remaining.tv_nsec=8;errno=77;
 CHECK(nanosleep(&zero,&remaining)==0 && errno==77 && remaining.tv_sec==7 && remaining.tv_nsec==8,36);
 struct timespec before, after;
 CHECK(clock_gettime(CLOCK_UPTIME_RAW,&before)==0,37);
 struct timespec delay;delay.tv_sec=0;delay.tv_nsec=2000000;
 CHECK(nanosleep(&delay,0)==0 && errno==77,38);
 CHECK(clock_gettime(CLOCK_UPTIME_RAW,&after)==0,39);
 CHECK((after.tv_sec-before.tv_sec)*1000000000LL+after.tv_nsec-before.tv_nsec>=2000000,40);
 return 0;
}

int v_macos_clock_probe_errno(void) { return errno; }
