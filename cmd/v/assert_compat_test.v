module main

fn test_v1_compat_assert_mode_detects_compiler_prefix() {
	assert v1_compat_assert_mode(['-assert', 'continues', 'sample_test.v']) or { '' } == 'continues'
	assert v1_compat_assert_mode(['-cc', 'clang', '-assert', 'aborts', 'sample.v']) or { '' } == 'aborts'
	assert v1_compat_assert_mode(['-assert', 'backtraces', 'sample.v']) or { '' } == 'backtraces'
}

fn test_v1_compat_assert_mode_ignores_nonlegacy_and_runtime_args() {
	assert v1_compat_assert_mode(['-assert', 'bogus', 'sample.v']) == none
	assert v1_compat_assert_mode(['run', 'sample.v', '-assert', 'continues']) == none
	assert v1_compat_assert_mode(['sample.v', '-assert', 'continues']) == none
}
