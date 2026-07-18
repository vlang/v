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

	termux_target := target_from('termux', 'aarch64') or { panic(err) }
	assert termux_target.os == 'termux'
	assert termux_target.arch == 'arm64'
	assert termux_target.abi == 'android'
	assert termux_target.object_format == 'elf'
}

fn test_native_arch_aliases_normalize_to_supported_targets() {
	assert target_from('linux', 'i386')!.arch == 'x86'
	assert target_from('linux', 'aarch32')!.arch == 'arm32'
	assert target_from('linux', 'rv64')!.arch == 'riscv64'
	assert target_from('linux', 'risc-v64')!.arch == 'riscv64'
	assert target_from('linux', 's390x')!.arch == 's390x'
	assert target_from('linux', 'ppc64')!.arch == 'ppc64'
	assert target_from('linux', 'ppc64le')!.arch == 'ppc64le'
	assert target_from('linux', 'loongarch64')!.arch == 'loongarch64'
	assert target_from('wasm32_emscripten', 'wasm')!.arch == 'wasm32'
}

fn test_remaining_native_os_targets_are_supported() {
	mut prefs := new_preferences()
	for os_name in ['qnx', 'haiku', 'serenity', 'vinix'] {
		target := target_from(os_name, 'amd64') or { panic(err) }
		assert target.os == os_name
		assert target.abi == 'gnu'
		assert target.object_format == 'elf'
		prefs.target = target
		assert comptime_flag_value(prefs, os_name)
		assert comptime_flag_value(prefs, 'posix')
	}
}

fn test_comptime_flags_use_target_instead_of_host() {
	mut prefs := new_preferences()
	prefs.target = target_from('linux', 's390x') or { panic(err) }
	assert comptime_flag_value(prefs, 'linux')
	assert comptime_flag_value(prefs, 's390x')
	assert comptime_flag_value(prefs, 'x64')
	assert comptime_flag_value(prefs, 'big_endian')
	assert !comptime_flag_value(prefs, 'macos')
	assert !comptime_flag_value(prefs, 'arm64')
	assert !comptime_flag_value(prefs, 'tinyc')

	prefs.target = target_from('termux', 'arm64') or { panic(err) }
	assert comptime_flag_value(prefs, 'termux')
	assert !comptime_flag_value(prefs, 'android')
	assert comptime_flag_value(prefs, 'posix')

	prefs.target = target_from('wasm32_emscripten', 'wasm32') or { panic(err) }
	assert comptime_flag_value(prefs, 'wasm32_emscripten')
	assert comptime_flag_value(prefs, 'wasm32')
	assert !comptime_flag_value(prefs, 'linux')

	prefs.target = target_from('linux', 'rv64') or { panic(err) }
	assert comptime_flag_value(prefs, 'rv64')
	assert comptime_flag_value(prefs, 'riscv64')
	prefs.target = target_from('linux', 'i386') or { panic(err) }
	assert comptime_flag_value(prefs, 'i386')
	assert comptime_flag_value(prefs, 'x86')
	assert comptime_flag_value(prefs, 'x32')
	prefs.target = target_from('linux', 'arm32') or { panic(err) }
	assert comptime_flag_value(prefs, 'arm32')
	assert comptime_flag_value(prefs, 'x32')
	prefs.target = target_from('wasm32_emscripten', 'wasm32') or { panic(err) }
	assert comptime_flag_value(prefs, 'wasm32')
	assert comptime_flag_value(prefs, 'x32')
}

fn test_debug_comptime_flag_uses_target_preferences() {
	mut prefs := new_preferences()
	assert !comptime_flag_value(prefs, 'debug')
	prefs.is_debug = true
	assert comptime_flag_value(prefs, 'debug')
}

fn test_source_selection_uses_target_os_and_arch() {
	dir := os.join_path(os.vtmp_dir(), 'v3_target_pref_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	for name in ['common.v', 'cpu_amd64.v', 'cpu_arm64.v', 'cpu.i386.v', 'cpu.i686.v', 'cpu.rv64.v',
		'sys_linux.v', 'sys_macos.v'] {
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

	linux_riscv64 := target_from('linux', 'riscv64') or { panic(err) }
	selected = get_v_files_from_dir_for_target(dir, [], linux_riscv64).map(os.base(it))
	selected.sort()
	assert selected == ['common.v', 'cpu.rv64.v', 'sys_linux.v']

	linux_x86 := target_from('linux', 'x86') or { panic(err) }
	selected = get_v_files_from_dir_for_target(dir, [], linux_x86).map(os.base(it))
	selected.sort()
	assert selected == ['common.v', 'cpu.i386.v', 'cpu.i686.v', 'sys_linux.v']
}

fn test_termux_source_selection_keeps_android_common_files_distinct() {
	dir := os.join_path(os.vtmp_dir(), 'v3_termux_target_pref_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	for name in ['platform_default.c.v', 'platform_android.c.v',
		'platform_android_outside_termux.c.v', 'platform_termux.c.v'] {
		os.write_file(os.join_path(dir, name), 'module sample\n') or { panic(err) }
	}

	termux := target_from('termux', 'arm64') or { panic(err) }
	mut selected := get_v_files_from_dir_for_target(dir, [], termux).map(os.base(it))
	selected.sort()
	assert selected == ['platform_android.c.v', 'platform_termux.c.v']

	android := target_from('android', 'arm64') or { panic(err) }
	selected = get_v_files_from_dir_for_target(dir, [], android).map(os.base(it))
	selected.sort()
	assert selected == ['platform_android.c.v', 'platform_android_outside_termux.c.v']

	linux := target_from('linux', 'arm64') or { panic(err) }
	selected = get_v_files_from_dir_for_target(dir, [], linux).map(os.base(it))
	assert selected == ['platform_default.c.v']
}

fn test_emscripten_source_and_test_selection_is_target_specific() {
	dir := os.join_path(os.vtmp_dir(), 'v3_emscripten_target_pref_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	for name in ['runtime_default.c.v', 'runtime_wasm32_emscripten.c.v'] {
		os.write_file(os.join_path(dir, name), 'module sample\n') or { panic(err) }
	}

	wasm := target_from('wasm32_emscripten', 'wasm32') or { panic(err) }
	assert get_v_files_from_dir_for_target(dir, [], wasm).map(os.base(it)) == [
		'runtime_wasm32_emscripten.c.v',
	]
	assert is_test_file_for_platform('/tmp/runtime_wasm32_emscripten_test.v', 'c', wasm)

	linux := target_from('linux', 'amd64') or { panic(err) }
	assert get_v_files_from_dir_for_target(dir, [], linux).map(os.base(it)) == [
		'runtime_default.c.v',
	]
	assert !is_test_file_for_platform('/tmp/runtime_wasm32_emscripten_test.v', 'c', linux)
}

fn test_remaining_native_os_source_selection_is_target_specific() {
	dir := os.join_path(os.vtmp_dir(), 'v3_native_os_target_pref_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	for name in ['platform_default.c.v', 'platform_qnx.c.v', 'platform_haiku.c.v',
		'platform_serenity.c.v', 'platform_vinix.c.v'] {
		os.write_file(os.join_path(dir, name), 'module sample\n') or { panic(err) }
	}

	for os_name in ['qnx', 'haiku', 'serenity', 'vinix'] {
		target := target_from(os_name, 'amd64') or { panic(err) }
		assert get_v_files_from_dir_for_target(dir, [], target).map(os.base(it)) == [
			'platform_${os_name}.c.v',
		]
	}
}
