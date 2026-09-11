module pref

import os

fn test_usable_bundled_tcc_compiler_skips_broken_binary() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	prepare_test_tcc_binary(test_root, 'exit 1')
	defer {
		os.rmdir_all(test_root) or {}
	}
	assert usable_bundled_tcc_compiler(test_root) == ''
}

fn test_usable_bundled_tcc_compiler_accepts_working_binary() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	tcc_path := prepare_test_tcc_binary(test_root, 'exit 0')
	defer {
		os.rmdir_all(test_root) or {}
	}
	assert usable_bundled_tcc_compiler(test_root) == tcc_path
}

fn test_usable_bundled_tcc_compiler_rejects_non_executable_file() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	tcc_path := prepare_test_tcc_binary(test_root, 'exit 0')
	os.chmod(tcc_path, 0o600) or { panic(err) }
	defer {
		os.rmdir_all(test_root) or {}
	}
	assert usable_bundled_tcc_compiler(test_root) == ''
}

fn test_try_to_use_tcc_by_default_keeps_explicit_system_tcc_on_musl() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	prepare_test_tcc_binary(test_root, 'exit 1')
	fake_vexe := os.join_path(test_root, 'v')
	old_vexe := os.getenv('VEXE')
	old_path := os.getenv('PATH')
	os.setenv('VEXE', fake_vexe, true)
	os.setenv('PATH', os.join_path(test_root, 'no_such_path'), true)
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	mut prefs := Preferences{
		ccompiler: 'tcc'
		is_musl: true
	}
	prefs.try_to_use_tcc_by_default()
	assert prefs.ccompiler == 'tcc'
}

fn test_try_to_use_tcc_by_default_skips_broken_bundled_tcc_on_musl() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	prepare_test_tcc_binary(test_root, 'exit 1')
	fake_vexe := os.join_path(test_root, 'v')
	old_vexe := os.getenv('VEXE')
	old_path := os.getenv('PATH')
	os.setenv('VEXE', fake_vexe, true)
	os.setenv('PATH', os.join_path(test_root, 'no_such_path'), true)
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	mut prefs := Preferences{
		is_musl: true
	}
	prefs.try_to_use_tcc_by_default()
	assert prefs.ccompiler == ''
}

fn test_try_to_use_tcc_by_default_skips_broken_bundled_tcc_off_musl() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	prepare_test_tcc_binary(test_root, 'exit 1')
	fake_vexe := os.join_path(test_root, 'v')
	old_vexe := os.getenv('VEXE')
	old_path := os.getenv('PATH')
	os.setenv('VEXE', fake_vexe, true)
	os.setenv('PATH', os.join_path(test_root, 'no_such_path'), true)
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	mut prefs := Preferences{}
	prefs.try_to_use_tcc_by_default()
	assert prefs.ccompiler == ''
}

fn test_try_to_use_tcc_by_default_skips_tcc_for_prealloc() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	prepare_test_tcc_binary(test_root, 'exit 0')
	fake_vexe := os.join_path(test_root, 'v')
	old_vexe := os.getenv('VEXE')
	os.setenv('VEXE', fake_vexe, true)
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
		os.rmdir_all(test_root) or {}
	}
	mut prefs := Preferences{
		prealloc: true
	}
	prefs.try_to_use_tcc_by_default()
	assert prefs.ccompiler == ''
}

fn test_try_to_use_tcc_by_default_does_not_probe_tcc_when_ineligible() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_ineligible_test')
	os.rmdir_all(test_root) or {}
	probe_marker := os.join_path(test_root, 'tcc_was_probed')
	system_tcc := prepare_test_executable(test_root, 'bin/tcc', 'printf probed > ${os.quoted_path(probe_marker)}\nexit 0')
	fake_vexe := os.join_path(test_root, 'v')
	old_vexe := os.getenv('VEXE')
	old_path := os.getenv('PATH')
	os.setenv('VEXE', fake_vexe, true)
	os.setenv('PATH', os.dir(system_tcc), true)
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	mut explicit_clang := Preferences{
		ccompiler: 'clang'
	}
	explicit_clang.try_to_use_tcc_by_default()
	assert explicit_clang.ccompiler == 'clang'
	assert !os.is_file(probe_marker)
	mut prod := Preferences{
		is_prod: true
	}
	prod.try_to_use_tcc_by_default()
	assert !os.is_file(probe_marker)
	mut prealloc := Preferences{
		prealloc: true
	}
	prealloc.try_to_use_tcc_by_default()
	assert !os.is_file(probe_marker)
	mut cross_target := Preferences{
		os: if get_host_os() == .linux { OS.windows } else { OS.linux }
	}
	cross_target.try_to_use_tcc_by_default()
	assert !os.is_file(probe_marker)
	mut cross_arch := Preferences{
		arch: if get_host_arch() == .amd64 { Arch.arm64 } else { Arch.amd64 }
	}
	cross_arch.try_to_use_tcc_by_default()
	assert !os.is_file(probe_marker)
	mut cross_c := Preferences{
		output_cross_c: true
	}
	cross_c.try_to_use_tcc_by_default()
	assert !os.is_file(probe_marker)
	mut js_backend := Preferences{
		backend: .js_node
	}
	js_backend.try_to_use_tcc_by_default()
	assert !os.is_file(probe_marker)
}

fn test_try_to_use_tcc_by_default_requires_glibc_tcc_runtime() {
	$if !linux {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_runtime_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	probe_marker := os.join_path(test_root, 'tcc_was_probed')
	system_tcc := prepare_test_executable(test_root, 'bin/tcc', 'printf probed > ${os.quoted_path(probe_marker)}\nexit 0')
	fake_vexe := os.join_path(test_root, 'v')
	old_vexe := os.getenv('VEXE')
	old_path := os.getenv('PATH')
	os.setenv('VEXE', fake_vexe, true)
	os.setenv('PATH', os.dir(system_tcc), true)
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	mut prefs := Preferences{
		os: .linux
		is_glibc: true
		gc_mode: .boehm_full_opt
	}
	prefs.try_to_use_tcc_by_default()
	assert prefs.ccompiler == ''
	assert !os.is_file(probe_marker)
	libgc := os.join_path(test_root, 'thirdparty', 'tcc', 'lib', 'libgc.a')
	os.mkdir_all(os.dir(libgc)) or { panic(err) }
	os.write_file(libgc, '') or { panic(err) }
	prefs.try_to_use_tcc_by_default()
	assert prefs.ccompiler == system_tcc
	assert os.is_file(probe_marker)
}

fn test_system_tcc_runtime_available_requires_bundled_boehm_archive() {
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_system_tcc_runtime_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	defer {
		os.rmdir_all(test_root) or {}
	}
	linux_prefs := Preferences{
		os: .linux
		is_glibc: true
		gc_mode: .boehm_full_opt
	}
	windows_prefs := Preferences{
		os: .windows
		gc_mode: .boehm_full_opt
	}
	assert !linux_prefs.system_tcc_runtime_available(test_root)
	assert !windows_prefs.system_tcc_runtime_available(test_root)
	libgc := os.join_path(test_root, 'thirdparty', 'tcc', 'lib', 'libgc.a')
	os.mkdir_all(os.dir(libgc)) or { panic(err) }
	os.write_file(libgc, '') or { panic(err) }
	assert linux_prefs.system_tcc_runtime_available(test_root)
	assert windows_prefs.system_tcc_runtime_available(test_root)
	assert Preferences{
		os: .linux
		is_musl: true
		gc_mode: .boehm_full_opt
	}.system_tcc_runtime_available(test_root)
	assert Preferences{
		os: .linux
		is_glibc: true
		gc_mode: .no_gc
	}.system_tcc_runtime_available(test_root)
}

fn test_try_to_use_tcc_by_default_skips_bundled_tcc_on_macos() {
	$if !macos {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	probe_marker := os.join_path(test_root, 'tcc_was_probed')
	prepare_test_tcc_binary(test_root, 'printf probed > ${os.quoted_path(probe_marker)}\nexit 0')
	fake_vexe := os.join_path(test_root, 'v')
	old_vexe := os.getenv('VEXE')
	os.setenv('VEXE', fake_vexe, true)
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
		os.rmdir_all(test_root) or {}
	}
	mut prefs := Preferences{
		vroot: test_root
		out_name: os.join_path(test_root, 'v')
	}
	prefs.try_to_use_tcc_by_default()
	assert prefs.ccompiler == ''
	assert !os.is_file(probe_marker)
}

fn test_usable_system_tcc_compiler_finds_tcc_from_path() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	system_tcc := prepare_test_executable(test_root, 'bin/tcc', 'exit 0')
	old_path := os.getenv('PATH')
	os.setenv('PATH', os.dir(system_tcc), true)
	defer {
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	assert usable_system_tcc_compiler() == system_tcc
}

fn test_usable_system_tcc_compiler_skips_broken_tcc_from_path() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	system_tcc := prepare_test_executable(test_root, 'bin/tcc', 'exit 1')
	old_path := os.getenv('PATH')
	os.setenv('PATH', os.dir(system_tcc), true)
	defer {
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	assert usable_system_tcc_compiler() == ''
}

fn test_default_tcc_compiler_uses_system_tcc_when_bundled_is_missing() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	fake_vexe := os.join_path(test_root, 'v')
	system_tcc := prepare_test_executable(test_root, 'bin/tcc', 'exit 0')
	old_vexe := os.getenv('VEXE')
	old_path := os.getenv('PATH')
	os.setenv('VEXE', fake_vexe, true)
	os.setenv('PATH', os.dir(system_tcc), true)
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	assert default_tcc_compiler() == system_tcc
	$if !macos {
		mut prefs := Preferences{}
		prefs.try_to_use_tcc_by_default()
		assert prefs.ccompiler == system_tcc
	}
}

fn test_try_to_use_tcc_by_default_resolves_explicit_tcc_to_system_tcc() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	fake_vexe := os.join_path(test_root, 'v')
	system_tcc := prepare_test_executable(test_root, 'bin/tcc', 'exit 0')
	old_vexe := os.getenv('VEXE')
	old_path := os.getenv('PATH')
	os.setenv('VEXE', fake_vexe, true)
	os.setenv('PATH', os.dir(system_tcc), true)
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	mut prefs := Preferences{
		ccompiler: 'tcc'
	}
	prefs.try_to_use_tcc_by_default()
	assert prefs.ccompiler == system_tcc
}

fn test_try_to_use_tcc_by_default_resolves_tinyc_alias_to_bundled_tcc() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	fake_vexe := os.join_path(test_root, 'v')
	bundled_tcc := prepare_test_tcc_binary(test_root, 'exit 0')
	old_vexe := os.getenv('VEXE')
	os.setenv('VEXE', fake_vexe, true)
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
		os.rmdir_all(test_root) or {}
	}
	mut prefs := Preferences{
		ccompiler: 'tinyc'
	}
	prefs.try_to_use_tcc_by_default()
	assert prefs.ccompiler == bundled_tcc
}

fn test_windows_default_c_compiler_prefers_gcc_when_available() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_c_compiler_test')
	prepare_test_tcc_binary(test_root, 'exit 0')
	fake_gcc := prepare_test_executable(test_root, 'bin/gcc', 'exit 0')
	old_path := os.getenv('PATH')
	os.setenv('PATH', os.dir(fake_gcc), true)
	defer {
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	assert windows_default_c_compiler(test_root) == 'gcc'
}

fn test_windows_default_c_compiler_falls_back_to_bundled_tcc_when_no_gcc() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_c_compiler_test')
	tcc_path := prepare_test_tcc_binary(test_root, 'exit 0')
	old_path := os.getenv('PATH')
	os.setenv('PATH', os.join_path(test_root, 'no_such_path'), true)
	defer {
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	assert windows_default_c_compiler(test_root) == tcc_path
}

fn test_windows_default_c_compiler_keeps_gcc_when_bundled_tcc_is_broken() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_c_compiler_test')
	prepare_test_tcc_binary(test_root, 'exit 1')
	old_path := os.getenv('PATH')
	os.setenv('PATH', os.join_path(test_root, 'no_such_path'), true)
	defer {
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	assert windows_default_c_compiler(test_root) == 'gcc'
}

fn test_windows_default_c_compiler_keeps_gcc_when_neither_is_available() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_c_compiler_test')
	os.rmdir_all(test_root) or {}
	old_path := os.getenv('PATH')
	os.setenv('PATH', os.join_path(test_root, 'no_such_path'), true)
	defer {
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	assert windows_default_c_compiler(test_root) == 'gcc'
}

fn prepare_test_executable(test_root string, relative_path string, exit_line string) string {
	path := os.join_path(test_root, relative_path)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, '#!/bin/sh\n${exit_line}\n') or { panic(err) }
	os.chmod(path, 0o700) or { panic(err) }
	return path
}

fn prepare_test_tcc_binary(test_root string, exit_line string) string {
	os.rmdir_all(test_root) or {}
	return prepare_test_executable(test_root, 'thirdparty/tcc/tcc.exe', exit_line)
}
