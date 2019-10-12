import runtime

fn test_nr_cpus() {
	nr_cpus := runtime.nr_cpus()
	assert nr_cpus > 0
}
