module cbuilder

fn test_parallel_cc_uses_tcc_for_resolved_compiler_paths() {
	assert parallel_cc_uses_tcc(.tcc, 'cc')
	assert parallel_cc_uses_tcc(.unknown, 'tcc')
	assert parallel_cc_uses_tcc(.unknown, '/tmp/v/thirdparty/tcc/tcc')
	assert parallel_cc_uses_tcc(.unknown, 'D:\\a\\v\\v\\thirdparty\\tcc\\tcc.exe')
	assert !parallel_cc_uses_tcc(.unknown, '/usr/bin/clang')
}

fn test_parallel_cc_projects_pkgconfig_pthread_once() {
	for projected in [
		parallel_cc_compile_driver_args(['-DISSUE74_COMPILE_ONLY'], true, .gcc, .gcc),
		parallel_cc_compile_driver_args(['-DISSUE74_COMPILE_ONLY'], true, .clang, .clang),
		parallel_cc_compile_driver_args(['-DISSUE74_COMPILE_ONLY'], true, .unknown, .cplusplus),
	] {
		assert projected == ['-DISSUE74_COMPILE_ONLY', '-pthread']
		assert projected.count(it == '-pthread') == 1
	}
	assert parallel_cc_compile_driver_args(['-pthread'], true, .gcc, .gcc) == [
		'-pthread',
	]
}

fn test_parallel_cc_does_not_project_pkgconfig_pthread_for_unsupported_compilers() {
	compile_args := ['-DISSUE74_COMPILE_ONLY', '-Wno-issue74-unrelated']
	assert parallel_cc_compile_driver_args(compile_args, true, .tcc, .tinyc) == compile_args
	assert parallel_cc_compile_driver_args(compile_args, true, .msvc, .msvc) == compile_args
	assert parallel_cc_compile_driver_args(compile_args, true, .unknown, .tinyc) == compile_args
}

fn test_parallel_cc_does_not_duplicate_combined_cflags_pthread() {
	combined_cflags := '-DISSUE74_CFLAGS=1 -pthread'
	assert parallel_cc_compile_driver_args([combined_cflags], true, .gcc, .gcc) == [
		combined_cflags,
	]
}

fn test_parallel_cc_does_not_duplicate_combined_environment_cflags_pthread() {
	combined_cflags := '-DISSUE74_ENV_CFLAGS=1 -pthread'
	assert parallel_cc_compile_driver_args([combined_cflags], true, .gcc, .gcc) == [
		combined_cflags,
	]
}

fn test_parallel_cc_does_not_project_ldflags_pthread() {
	assert parallel_cc_compile_driver_args(['-DISSUE74_COMPILE_ONLY'], false, .gcc, .gcc) == [
		'-DISSUE74_COMPILE_ONLY',
	]
}

fn test_parallel_cc_does_not_project_environment_ldflags_pthread() {
	assert parallel_cc_compile_driver_args(['-DISSUE74_COMPILE_ONLY'], false, .gcc, .gcc) == [
		'-DISSUE74_COMPILE_ONLY',
	]
}
