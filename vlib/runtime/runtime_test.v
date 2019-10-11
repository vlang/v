import runtime

fn test_nr_cpus() {
	$if linux {
		nr_cpus := runtime.nr_cpus()
		assert nr_cpus > 0
	}
	$if windows {
		nr_cpus := runtime.nr_cpus()
		assert nr_cpus > 0
	}
}
