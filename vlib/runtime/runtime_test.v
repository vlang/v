import runtime

fn test_nr_cpus() {
	nr_cpus := runtime.nr_cpus()
	assert nr_cpus > 0
}

fn test_nr_jobs() {
	nr_jobs := runtime.nr_jobs()
	assert nr_jobs > 0
}

fn test_is_32bit() {
	x := runtime.is_32bit().str()
	assert x == 'true' || x == 'false'
}

fn test_is_64bit() {
	x := runtime.is_64bit().str()
	assert x == 'true' || x == 'false'
}

fn test_is_little_endian() {
	x := runtime.is_little_endian().str()
	assert x == 'true' || x == 'false'
}

fn test_is_big_endian() {
	x := runtime.is_big_endian().str()
	assert x == 'true' || x == 'false'
}

fn test_is_big_endian_different_than_is_little_endian() {
	assert runtime.is_big_endian() != runtime.is_little_endian()
}

fn test_is_32bit_different_than_is_64bit() {
	assert runtime.is_32bit() != runtime.is_64bit()
}
