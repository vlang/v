module fastc

#include <unistd.h>

fn C.sysconf(name int) i64

// fastc_nr_cpus reports the host's online CPU count for parallel generation.
fn fastc_nr_cpus() int {
	// sysconf is a libc call; validate its result before using it as a count.
	count := unsafe { int(C.sysconf(C._SC_NPROCESSORS_ONLN)) }
	if count < 1 {
		return 1
	}
	return count
}
