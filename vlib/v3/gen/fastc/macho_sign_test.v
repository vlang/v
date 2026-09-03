module fastc

import os

// A TinyCC-linked executable signed in process must run and pass Apple's
// signature verification.
fn test_fastc_sign_macho_adhoc_matches_codesign() {
	if os.user_os() != 'macos' {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	test_dir := os.join_path(os.temp_dir(), 'fastc_macho_sign_${os.getpid()}')
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	source_path := os.join_path_single(test_dir, 'hello.c')
	exe_path := os.join_path_single(test_dir, 'hello')
	os.write_file(source_path, 'int puts(const char *);\nint main(void) { puts("signed"); return 0; }\n') or {
		panic(err)
	}
	tcc_lib := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'lib')
	mut args := ['-std=gnu11', '-B${tcc_lib}', '-I${os.join_path_single(tcc_lib, 'include')}',
		'-L${tcc_lib}']
	mut sdk_root := os.getenv('SDKROOT')
	if !os.is_dir(sdk_root) {
		result := os.execute('xcrun --show-sdk-path')
		if result.exit_code == 0 {
			sdk_root = result.output.trim_space()
		}
	}
	if os.is_dir(sdk_root) {
		args << '-L${os.join_path(sdk_root, 'usr', 'lib')}'
	}
	args << ['-w', '-o', exe_path, source_path]
	// Without `codesign` on PATH TinyCC leaves the executable unsigned (and
	// reports the failed call), which is what the signer is given.
	old_path := os.getenv('PATH')
	os.setenv('PATH', '/nonexistent', true)
	os.execute('${tcc} ${args.join(' ')}')
	os.setenv('PATH', old_path, true)
	assert os.is_file(exe_path)
	unsigned := os.execute(exe_path)
	assert unsigned.exit_code != 0
	fastc_sign_macho_adhoc(exe_path) or { panic(err) }
	signed := os.execute(exe_path)
	assert signed.exit_code == 0, signed.output
	assert signed.output.trim_space() == 'signed'
	verify := os.execute('codesign --verify --strict ${exe_path}')
	assert verify.exit_code == 0, verify.output
	// Signing again replaces the signature.
	fastc_sign_macho_adhoc(exe_path) or { panic(err) }
	again := os.execute('codesign --verify --strict ${exe_path}')
	assert again.exit_code == 0, again.output
	// Re-signing under a shorter name replaces a larger signature (the
	// identifier is the file name): the file shrinks and still verifies.
	long_path := os.join_path_single(test_dir, 'hello_with_a_much_longer_file_name')
	os.cp(exe_path, long_path) or { panic(err) }
	fastc_sign_macho_adhoc(long_path) or { panic(err) }
	long_size := os.file_size(long_path)
	os.mv(long_path, exe_path) or { panic(err) }
	fastc_sign_macho_adhoc(exe_path) or { panic(err) }
	assert os.file_size(exe_path) < long_size
	shrunk := os.execute('codesign --verify --strict ${exe_path}')
	assert shrunk.exit_code == 0, shrunk.output
	assert os.execute(exe_path).output.trim_space() == 'signed'
}
