import runtime

fn test_nr_cpus() {
	mut do_test := false
	$if linux {
		do_test = true
	}
	$if windows {
		do_test = true
	}
	if do_test {
		nr_cpus := runtime.nr_cpus()
		println(nr_cpus)
		assert nr_cpus > 0
	}
}
