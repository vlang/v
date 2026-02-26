module builder

fn test_is_tcc_compilation_failure_detects_tcc_kind() {
	assert is_tcc_compilation_failure('cc', .tcc, '')
}

fn test_is_tcc_compilation_failure_detects_tcc_compiler_name() {
	assert is_tcc_compilation_failure('tcc', .unknown, '')
	assert is_tcc_compilation_failure('/opt/v/thirdparty/tcc/tcc.exe', .unknown, '')
	assert is_tcc_compilation_failure('/usr/local/bin/tcc-0.9.27', .unknown, '')
	assert !is_tcc_compilation_failure('/usr/bin/clang', .unknown, '')
}

fn test_is_tcc_compilation_failure_detects_tcc_output() {
	assert is_tcc_compilation_failure('cc', .unknown, 'tcc: error: bad architecture')
	assert is_tcc_compilation_failure('cc', .unknown, 'line 1\nline 2\ntcc: error: lib not found')
	assert !is_tcc_compilation_failure('cc', .unknown, 'clang: error: unsupported option')
}
