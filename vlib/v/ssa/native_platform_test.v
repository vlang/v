module ssa

fn test_native_linux_constants_do_not_inherit_different_darwin_values() {
	for name, value in {
		'PTHREAD_CREATE_JOINABLE': i64(0)
		'PTHREAD_CREATE_DETACHED': i64(1)
		'PTHREAD_PROCESS_PRIVATE': i64(0)
		'PTHREAD_PROCESS_SHARED':  i64(1)
		'PATH_MAX':                i64(4096)
		'O_SYNC':                  i64(1052672)
		'O_FSYNC':                 i64(1052672)
	} {
		assert native_linux_constant(name)? == value, name
	}
	assert native_darwin_constant('PTHREAD_CREATE_JOINABLE')? == 1
	assert native_darwin_constant('PTHREAD_CREATE_DETACHED')? == 2
	assert native_darwin_constant('PTHREAD_PROCESS_PRIVATE')? == 2
	assert native_darwin_constant('PATH_MAX')? == 1024
	assert native_darwin_constant('O_FSYNC')? == 128
}
