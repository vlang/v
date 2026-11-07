module machosign

import os

fn test_codesign_shim_restores_path() {
	if os.user_os() != 'macos' {
		return
	}
	previous_path := os.getenv_opt('PATH')
	shim := codesign_shim_dir()
	if shim.dir == '' {
		return
	}
	assert os.getenv('PATH').starts_with(shim.dir + ':')
	remove_codesign_shim_dir(shim)
	assert os.getenv_opt('PATH') == previous_path
	assert !os.exists(shim.dir)
}

// A TinyCC-linked executable signed in process must run and pass Apple's
// signature verification.
fn test_sign_adhoc_matches_codesign() {
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
		result := os.exec(['xcrun', '--show-sdk-path'])
		if result.exit_code == 0 {
			sdk_root = result.output.trim_space()
		}
	}
	if os.is_dir(sdk_root) {
		args << '-L${os.join_path(sdk_root, 'usr', 'lib')}'
	}
	args << ['-w', '-o', exe_path, source_path]
	compiled := os.exec([tcc, ...args])
	assert compiled.exit_code == 0, compiled.output
	assert os.is_file(exe_path)
	// Create the unsigned fixture explicitly, independent of TinyCC's signing
	// method. Sign first so removal also works with bundles that do not sign.
	prepared := os.exec(['codesign', '--force', '--sign', '-', exe_path])
	assert prepared.exit_code == 0, prepared.output
	removed := os.exec(['codesign', '--remove-signature', exe_path])
	assert removed.exit_code == 0, removed.output
	unsigned_signature := os.exec(['codesign', '--verify', '--strict', exe_path])
	assert unsigned_signature.exit_code != 0, unsigned_signature.output
	// Signature verification establishes the fixture's state. Whether an
	// unsigned executable can run depends on the host's execution policy.
	sign_adhoc(exe_path) or { panic(err) }
	signed := os.exec([exe_path])
	assert signed.exit_code == 0, signed.output
	assert signed.output.trim_space() == 'signed'
	verify := os.exec(['codesign', '--verify', '--strict', exe_path])
	assert verify.exit_code == 0, verify.output
	// Signing again replaces the signature.
	sign_adhoc(exe_path) or { panic(err) }
	again := os.exec(['codesign', '--verify', '--strict', exe_path])
	assert again.exit_code == 0, again.output
	// Re-signing under a shorter name replaces a larger signature (the
	// identifier is the file name): the file shrinks and still verifies.
	long_path := os.join_path_single(test_dir, 'hello_with_a_much_longer_file_name')
	os.cp(exe_path, long_path) or { panic(err) }
	sign_adhoc(long_path) or { panic(err) }
	long_size := os.file_size(long_path)
	os.mv(long_path, exe_path) or { panic(err) }
	sign_adhoc(exe_path) or { panic(err) }
	assert os.file_size(exe_path) < long_size
	shrunk := os.exec(['codesign', '--verify', '--strict', exe_path])
	assert shrunk.exit_code == 0, shrunk.output
	assert os.exec([exe_path]).output.trim_space() == 'signed'
}

// TinyCC records macOS 10.6 as the SDK, so AppKit lays out its controls as for a
// 2009 program. Signing records the running macOS instead; dyld then wants the UUID
// and no repeated rpath of a platform-linked program.
fn test_sign_adhoc_records_the_host_sdk() {
	if os.user_os() != 'macos' {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	host_version := fastc_host_macos_version()
	if !os.is_file(tcc) || host_version == 0 {
		return
	}
	test_dir := os.join_path(os.temp_dir(), 'fastc_macho_sdk_${os.getpid()}')
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	source_path := os.join_path_single(test_dir, 'hello.c')
	exe_path := os.join_path_single(test_dir, 'hello')
	os.write_file(source_path, 'int puts(const char *);\nint main(void) { puts("modern"); return 0; }\n') or {
		panic(err)
	}
	tcc_lib := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'lib')
	mut args := ['-std=gnu11', '-B${tcc_lib}', '-I${os.join_path_single(tcc_lib, 'include')}',
		'-L${tcc_lib}']
	mut sdk_root := os.getenv('SDKROOT')
	if !os.is_dir(sdk_root) {
		result := os.exec(['xcrun', '--show-sdk-path'])
		if result.exit_code == 0 {
			sdk_root = result.output.trim_space()
		}
	}
	if os.is_dir(sdk_root) {
		args << '-L${os.join_path(sdk_root, 'usr', 'lib')}'
	}
	args << ['-w', '-Wl,-rpath,${test_dir}', '-Wl,-rpath,${test_dir}', '-o', exe_path, source_path]
	compiled := os.exec([tcc, ...args])
	assert compiled.exit_code == 0, compiled.output
	sign_adhoc(exe_path) or { panic(err) }
	run := os.exec([exe_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'modern'
	verify := os.exec(['codesign', '--verify', '--strict', exe_path])
	assert verify.exit_code == 0, verify.output
	commands := os.exec(['otool', '-l', exe_path])
	assert commands.exit_code == 0, commands.output
	major := host_version >> 16
	minor := (host_version >> 8) & 0xff
	assert commands.output.contains('sdk ${major}.${minor}'), commands.output
	assert commands.output.contains('cmd LC_UUID'), commands.output
	assert commands.output.count('cmd LC_RPATH') == 1, commands.output
}
