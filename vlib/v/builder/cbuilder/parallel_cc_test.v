module cbuilder

import os

fn test_parallel_cc_bundled_tcc_root_excludes_system_tcc() {
	vroot := os.join_path(os.vtmp_dir(), 'parallel_cc_bundled_tcc_root')
	bundled_tcc_root := os.join_path(vroot, 'thirdparty', 'tcc')
	bundled_tcc := os.join_path(bundled_tcc_root, 'tcc.exe')
	system_tcc := os.join_path(vroot, 'usr', 'bin', 'tcc')
	assert parallel_cc_bundled_tcc_root(vroot, bundled_tcc) == bundled_tcc_root
	assert parallel_cc_bundled_tcc_root(vroot, system_tcc) == ''
	assert parallel_cc_bundled_tcc_root(vroot, 'clang') == ''
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
