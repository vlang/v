module cbuilder

fn test_parallel_cc_uses_tcc_for_resolved_compiler_paths() {
	assert parallel_cc_uses_tcc(.tcc, 'cc')
	assert parallel_cc_uses_tcc(.unknown, 'tcc')
	assert parallel_cc_uses_tcc(.unknown, '/tmp/v/thirdparty/tcc/tcc')
	assert parallel_cc_uses_tcc(.unknown, 'D:\\a\\v\\v\\thirdparty\\tcc\\tcc.exe')
	assert !parallel_cc_uses_tcc(.unknown, '/usr/bin/clang')
}
