module runtime

fn C.sysconf(name int) i64

// nr_cpus returns the number of virtual CPU cores found on the system.
pub fn nr_cpus() int {
	$if linux || macos || solaris {
		return int(C.sysconf(C._SC_NPROCESSORS_ONLN))
	}
	return 1
}
