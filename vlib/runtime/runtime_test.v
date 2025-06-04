import runtime

fn test_physical_memory() {
	$if windows || linux || darwin || freebsd || openbsd {
		total := runtime.total_memory()!
		free := runtime.free_memory()!
		println('total memory: ${total}')
		println('free  memory: ${free}')
		assert total > 0 && free > 0
	} $else {
		total := runtime.total_memory()!
		_ := runtime.free_memory() or { assert err.msg().contains('not implemented') }
		assert total > 0
	}
}

fn test_nr_cpus() {
	nr_cpus := runtime.nr_cpus()
	println('     nr cpus: ${nr_cpus}')
	assert nr_cpus > 0
}

fn test_nr_jobs() {
	nr_jobs := runtime.nr_jobs()
	println('     nr jobs: ${nr_jobs}')
	assert nr_jobs > 0
}

fn test_is_32bit() {
	x := runtime.is_32bit().str()
	println('    is_32bit: ${x}')
	assert x == 'true' || x == 'false'
}

fn test_is_64bit() {
	x := runtime.is_64bit().str()
	println('    is_64bit: ${x}')
	assert x == 'true' || x == 'false'
}

fn test_is_little_endian() {
	x := runtime.is_little_endian().str()
	println('       is_le: ${x}')
	assert x == 'true' || x == 'false'
}

fn test_is_big_endian() {
	x := runtime.is_big_endian().str()
	println('       is_be: ${x}')
	assert x == 'true' || x == 'false'
}

fn test_is_big_endian_different_than_is_little_endian() {
	assert runtime.is_big_endian() != runtime.is_little_endian()
}

fn test_is_32bit_different_than_is_64bit() {
	assert runtime.is_32bit() != runtime.is_64bit()
}
