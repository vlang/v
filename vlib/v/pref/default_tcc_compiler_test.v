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
		ccompiler: 'tcc'
		is_musl:   true
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
	os.setenv('VEXE', fake_vexe, true)
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
		os.rmdir_all(test_root) or {}
	}
	mut prefs := Preferences{}
	prefs.try_to_use_tcc_by_default()
	assert prefs.ccompiler == ''
}

fn test_usable_system_tcc_compiler_prefers_termux_tcc_from_path() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	system_tcc := prepare_test_executable(test_root, 'bin/tcc', 'exit 0')
	old_path := os.getenv('PATH')
	old_termux_version := os.getenv('TERMUX_VERSION')
	os.setenv('PATH', os.dir(system_tcc), true)
	os.setenv('TERMUX_VERSION', '0.118.0', true)
	defer {
		os.setenv('PATH', old_path, true)
		if old_termux_version == '' {
			os.unsetenv('TERMUX_VERSION')
		} else {
			os.setenv('TERMUX_VERSION', old_termux_version, true)
		}
		os.rmdir_all(test_root) or {}
	}
	assert usable_system_tcc_compiler() == system_tcc
}

fn test_default_tcc_compiler_uses_system_tcc_on_termux_when_bundled_is_missing() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	fake_vexe := os.join_path(test_root, 'v')
	system_tcc := prepare_test_executable(test_root, 'bin/tcc', 'exit 0')
	old_vexe := os.getenv('VEXE')
	old_path := os.getenv('PATH')
	old_termux_version := os.getenv('TERMUX_VERSION')
	os.setenv('VEXE', fake_vexe, true)
	os.setenv('PATH', os.dir(system_tcc), true)
	os.setenv('TERMUX_VERSION', '0.118.0', true)
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
		os.setenv('PATH', old_path, true)
		if old_termux_version == '' {
			os.unsetenv('TERMUX_VERSION')
		} else {
			os.setenv('TERMUX_VERSION', old_termux_version, true)
		}
		os.rmdir_all(test_root) or {}
	}
	assert default_tcc_compiler() == system_tcc
}

fn test_try_to_use_tcc_by_default_resolves_explicit_tcc_to_system_tcc_on_termux() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_default_tcc_compiler_test')
	fake_vexe := os.join_path(test_root, 'v')
	system_tcc := prepare_test_executable(test_root, 'bin/tcc', 'exit 0')
	old_vexe := os.getenv('VEXE')
	old_path := os.getenv('PATH')
	old_termux_version := os.getenv('TERMUX_VERSION')
	os.setenv('VEXE', fake_vexe, true)
	os.setenv('PATH', os.dir(system_tcc), true)
	os.setenv('TERMUX_VERSION', '0.118.0', true)
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
		os.setenv('PATH', old_path, true)
		if old_termux_version == '' {
			os.unsetenv('TERMUX_VERSION')
		} else {
			os.setenv('TERMUX_VERSION', old_termux_version, true)
		}
		os.rmdir_all(test_root) or {}
	}
	mut prefs := Preferences{
		ccompiler: 'tcc'
	}
	prefs.try_to_use_tcc_by_default()
	assert prefs.ccompiler == system_tcc
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
