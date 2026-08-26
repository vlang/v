module fastc

#include <unistd.h>

fn C.sysconf(name int) i64

// fastc_nr_cpus reports the host's online CPU count for parallel generation.
fn fastc_nr_cpus() int {
	count := int(C.sysconf(C._SC_NPROCESSORS_ONLN))
	if count < 1 {
		return 1
	}
	return count
}
