module runtime

pub fn nr_cpus() int {
	$if linux {
		return int(C.sysconf(C._SC_NPROCESSORS_ONLN))
	}
	$if macos {
		return int(C.sysconf(C._SC_NPROCESSORS_ONLN))
	}
	$if solaris {
		return int(C.sysconf(C._SC_NPROCESSORS_ONLN))
	}
	return 1
}