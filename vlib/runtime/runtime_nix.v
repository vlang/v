module runtime

fn nr_cpus_nix() int {
	$if linux {
		return int(C.sysconf(C._SC_NPROCESSORS_ONLN))
	}
	$if mac {
		return int(C.sysconf(C._SC_NPROCESSORS_ONLN))
	}
	return 1
}

fn nr_cpus_win() int {
	eprintln('nr_cpus_win should be callable only for windows')
	return 1
}
