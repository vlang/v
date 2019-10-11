module runtime

$if linux {
	#include <sys/sysinfo.h>
	fn C.get_nprocs() int
}


pub fn nr_cpus() int {
	$if linux {
		return C.get_nprocs()
	}
	return 0
}
