module bench

#include <sys/resource.h>

struct C.rusage {
	ru_maxrss i64
}

fn C.getrusage(who int, usage &C.rusage) int

fn peak_rss_kb() i64 {
	mut usage := C.rusage{}
	if C.getrusage(C.RUSAGE_SELF, &usage) != 0 {
		return current_rss_kb()
	}
	$if macos {
		return usage.ru_maxrss / 1024
	} $else {
		return usage.ru_maxrss
	}
}
