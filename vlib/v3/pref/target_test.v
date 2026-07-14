module pref

import os

fn test_target_from_normalizes_and_derives_platform_properties() {
	target := target_from('darwin', 'x86_64') or { panic(err) }
	assert target.os == 'macos'
	assert target.arch == 'amd64'
	assert target.abi == 'darwin'
	assert target.endian == 'little'
	assert target.pointer_bits == 64
	assert target.object_format == 'macho'

	wasm_target := target_from('emscripten', 'wasm32') or { panic(err) }
	assert wasm_target.pointer_bits == 32
	assert wasm_target.object_format == 'wasm'
	if _ := target_from('emscripten', 'arm64') {
		assert false
	} else {
		assert true
	}
}

fn test_comptime_flags_use_target_instead_of_host() {
	mut prefs := new_preferences()
	prefs.target = target_from('linux', 's390x') or { panic(err) }
	assert comptime_flag_value(prefs, 'linux')
	assert comptime_flag_value(prefs, 's390x')
	assert comptime_flag_value(prefs, 'big_endian')
	assert !comptime_flag_value(prefs, 'macos')
	assert !comptime_flag_value(prefs, 'arm64')
	assert !comptime_flag_value(prefs, 'tinyc')
}

fn test_source_selection_uses_target_os_and_arch() {
	dir := os.join_path(os.vtmp_dir(), 'v3_target_pref_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	for name in ['common.v', 'cpu_amd64.v', 'cpu_arm64.v', 'sys_linux.v', 'sys_macos.v'] {
		os.write_file(os.join_path(dir, name), 'module sample\n') or { panic(err) }
	}

	linux_arm64 := target_from('linux', 'arm64') or { panic(err) }
	mut selected := get_v_files_from_dir_for_target(dir, [], linux_arm64).map(os.base(it))
	selected.sort()
	assert selected == ['common.v', 'cpu_arm64.v', 'sys_linux.v']

	macos_amd64 := target_from('macos', 'amd64') or { panic(err) }
	selected = get_v_files_from_dir_for_target(dir, [], macos_amd64).map(os.base(it))
	selected.sort()
	assert selected == ['common.v', 'cpu_amd64.v', 'sys_macos.v']
}
