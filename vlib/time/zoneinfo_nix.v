module time

#include <sys/time.h>

struct UnixTimeZone{
	tv C.timeval
	tz C.timezone
}

fn get_system_timezone(){
	C.gettimeofday(&tv,&tz)
}


struct C.timeval
struct C.timezone