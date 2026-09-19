module driver

import os

fn produced_binary_test_dir(name string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_produced_binary_${name}_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	return dir
}

fn test_v3_produced_binary_prefers_the_requested_name() {
	dir := produced_binary_test_dir('exact')
	defer {
		os.rmdir_all(dir) or {}
	}
	cc_out := os.join_path(dir, 'out')
	os.write_file(cc_out, 'tcc')!
	// A suffixed sibling must not shadow the file the compiler was asked for.
	os.write_file(cc_out + '.exe', 'stale')!
	assert v3_produced_binary(cc_out) == cc_out
}

fn test_v3_produced_binary_accepts_the_platform_suffix() {
	// MinGW gcc and llvm-mingw clang append `.exe` to an extension-less `-o out`
	// target (and `.dll` to a shared library), while TinyCC writes `out` as is.
	for suffix in ['.exe', '.dll'] {
		dir := produced_binary_test_dir('suffix' + suffix.trim_left('.'))
		cc_out := os.join_path(dir, 'out')
		os.write_file(cc_out + suffix, 'gcc')!
		assert v3_produced_binary(cc_out) == cc_out + suffix
		os.rmdir_all(dir) or {}
	}
}

fn test_v3_produced_binary_keeps_a_missing_output_missing() {
	dir := produced_binary_test_dir('missing')
	defer {
		os.rmdir_all(dir) or {}
	}
	cc_out := os.join_path(dir, 'out')
	// Nothing was produced: the caller must still see (and report) the
	// requested path, not an invented one.
	assert v3_produced_binary(cc_out) == cc_out
	assert !os.exists(v3_produced_binary(cc_out))
}
