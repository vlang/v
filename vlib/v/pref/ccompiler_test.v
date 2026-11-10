module pref

import os

fn test_ccompiler_can_assemble_rejects_a_tcc_alias() {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'v_pref_assembler_${os.getpid()}')
	os.mkdir_all(test_root) or { panic(err) }
	old_path := os.getenv('PATH')
	defer {
		os.setenv('PATH', old_path, true)
		os.rmdir_all(test_root) or {}
	}
	alias_path := os.join_path(test_root, 'cc')
	os.write_file(alias_path, '#!/bin/sh\nprintf "tcc version 0.9.27 (x86_64 Linux)\\n"\n') or {
		panic(err)
	}
	os.chmod(alias_path, 0o700) or { panic(err) }
	assert !ccompiler_can_assemble(alias_path)
	os.setenv('PATH', test_root, true)
	assert find_system_assembler() == none

	os.write_file(alias_path, '#!/bin/sh\nprintf "OpenBSD clang version 16.0.6\\n"\n') or {
		panic(err)
	}
	assert ccompiler_can_assemble(alias_path)
	found := find_system_assembler() or { panic('the fake Clang was not found') }
	assert found == alias_path
}

fn test_tcc_with_prod_requested_needs_both_prod_and_a_tinycc_name() {
	assert tcc_with_prod_requested(['-prod', '-cc', 'tcc', 'main.v'])
	assert tcc_with_prod_requested(['-cc', 'C:/tools/tcc.exe', '-prod', 'main.v'])
	assert tcc_with_prod_requested(['-cc=tinyc', '-prod', 'main.v'])
	assert tcc_with_prod_requested(['-prod', '-cc', 'TCC.EXE', 'main.v'])
	assert !tcc_with_prod_requested(['-cc', 'tcc', 'main.v'])
	assert !tcc_with_prod_requested(['-prod', '-cc', 'clang', 'main.v'])
	assert !tcc_with_prod_requested(['-prod', '-cc'])
	assert !tcc_with_prod_requested(['-prod', 'main.v'])
	// The last `-cc` wins, so a `VFLAGS=-cc tcc` can be overridden on the command line.
	assert !tcc_with_prod_requested(['-cc', 'tcc', '-prod', '-cc', 'gcc', 'main.v'])
	assert tcc_with_prod_requested(['-cc', 'gcc', '-prod', '-cc', 'tcc', 'main.v'])
}

fn test_tcc_prod_detection_does_not_match_unrelated_names_or_backends() {
	assert !tcc_with_prod_requested(['-prod', '-cc', 'distcc'])
	assert !tcc_with_prod_requested(['-prod', '-cc', 'notcc'])
	assert !tcc_with_prod_requested(['-prod', '-cc', 'tcc', '-b', 'arm64'])
	assert !tcc_with_prod_requested(['-prod', '-cc', 'tcc', '-b', 'fastc'])
	assert tcc_with_prod_requested(['-prod', '-cc', 'tcc', '-b', 'arm64', '-b', 'c'])
	assert !tcc_with_prod_requested(['-cc', '-prod'])
}

fn test_tcc_prod_detection_checks_compiler_aliases() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_pref_prod_alias_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	alias := os.join_path(root, 'cc')
	os.write_file(alias, '#!/bin/sh\nprintf "tcc version 0.9.27\\n"\n')!
	os.chmod(alias, 0o700)!
	assert tcc_with_prod_requested(['-prod', '-cc', alias])
	os.write_file(alias, '#!/bin/sh\nprintf "clang version 18.0.0\\n"\n')!
	assert !tcc_with_prod_requested(['-prod', '-cc', alias])
	os.write_file(alias, '#!/bin/sh\nprintf "clang version 19.1.7\\nInstalledDir: /opt/tinycc-tcc version-compatible-sdk/bin\\n"\n')!
	assert !ccompiler_is_tinycc(alias)
	assert !tcc_with_prod_requested(['-prod', '-cc', alias])
	assert ccompiler_can_assemble(alias)
}

fn test_ccompiler_version_is_tinycc_recognizes_banner_lines_only() {
	for banner in ['tcc version 0.9.27 (arm64 Darwin)', 'TinyCC version 0.9.27',
		'Tiny C Compiler 0.9.27', 'Tiny C Compiler - Copyright (C) 2001-2026',
		'warning: wrapper\n  tcc version 0.9.27\n', 'TinyCC'] {
		assert ccompiler_version_is_tinycc(banner), banner
	}
	for version in ['clang version 19.1.7\nInstalledDir: /opt/tinycc-compatible-sdk/bin',
		'clang version 19.1.7\nInstalledDir: /opt/tcc version-compatible-sdk/bin',
		'gcc version 14.0\nConfigured with: --prefix=/opt/tiny c compiler-sdk',
		'clang version 19.1.7 (supports TinyCC-compatible inputs)', 'notcc version 1.0'] {
		assert !ccompiler_version_is_tinycc(version), version
	}
}

fn test_tinycc_file_name_matches_tcc_names_only() {
	for name in ['tcc', 'tcc.exe', 'TCC.EXE', 'tinyc', 'tinycc', 'x86_64-w64-mingw32-tcc'] {
		assert tinycc_file_name(name), name
	}
	for name in ['distcc', 'notcc', 'notcc.exe', 'gcc', 'clang', 'cc'] {
		assert !tinycc_file_name(name), name
	}
}

fn test_tcc_with_prod_requested_finds_tinycc_under_another_name() {
	bundled := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(bundled) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_pref_tinycc_alias_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	alias_path := os.join_path(root, 'cc_alias.exe')
	os.cp(bundled, alias_path) or { panic(err) }
	// Shared TinyCC builds need their adjacent DLL even to print the version.
	libtcc := os.join_path(os.dir(bundled), 'libtcc.dll')
	if os.is_file(libtcc) {
		os.cp(libtcc, os.join_path(root, 'libtcc.dll')) or { panic(err) }
	}
	assert tcc_with_prod_requested(['-prod', '-cc', alias_path])
	assert !tcc_with_prod_requested(['-cc', alias_path])
}

fn test_tcc_with_prod_requested_ignores_other_executables_with_tcc_in_their_name() {
	root := os.join_path(os.vtmp_dir(), 'v_pref_notcc_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	// The V executable is a real compiler driver that is not TinyCC.
	not_tinycc := os.join_path(root, 'notcc.exe')
	os.cp(@VEXE, not_tinycc) or { panic(err) }
	assert !tcc_with_prod_requested(['-prod', '-cc', not_tinycc])
	assert !tcc_with_prod_requested(['-prod', '-cc', 'distcc'])
}

fn test_tcc_prod_detection_skips_option_values() {
	assert !tcc_with_prod_requested(['-cc', 'tcc', '-o', '-prod', 'main.v'])
	assert !tcc_with_prod_requested(['-prod', '-cc', 'clang', '-o', '-cc=tcc', 'main.v'])
	assert tcc_with_prod_requested(['-prod', '-cc', 'tcc', '-o', '-b=arm64', 'main.v'])
	assert tcc_with_prod_requested(['-profile', '-prod', '-cc', 'tcc', 'main.v'])
	assert tcc_with_prod_requested(['-profile', 'report.txt', '-prod', '-cc', 'tcc', 'main.v'])
}

fn test_tcc_prod_detection_uses_recorded_wrapper_directory_without_chdir() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_pref_cwd_${os.getpid()}')
	project := os.join_path(root, 'tinycc-project')
	os.mkdir_all(project)!
	defer { os.rmdir_all(root) or {} }
	wrapper := os.join_path(root, 'wrapper')
	os.write_file(wrapper, '#!/bin/sh\ncase "$PWD" in\n*/tinycc-project) echo "tcc version 0.9.27" ;;\n*) echo "clang version 19.1.7" ;;\nesac\n')!
	os.chmod(wrapper, 0o700)!
	before := os.getwd()
	for args in [['-prod', '-cc', wrapper], ['-prod', '-cc', '../wrapper'],
		['-prod', '-cc=../wrapper']] {
		assert tcc_with_prod_requested_in(args, project)
	}
	assert !tcc_with_prod_requested_in(['-prod', '-cc', wrapper], root)
	assert !tcc_with_prod_requested_in(['-cc', wrapper], project)
	assert !tcc_with_prod_requested_in(['-cc', wrapper, '-o', '-prod'], project)
	assert os.getwd() == before
	bare := 'pref_context_cc_${os.getpid()}'
	os.cp(wrapper, os.join_path(project, bare))!
	os.chmod(os.join_path(project, bare), 0o700)!
	saved_path := os.getenv('PATH')
	defer { os.setenv('PATH', saved_path, true) }
	for prefix in ['.', ''] {
		os.setenv('PATH', prefix + os.path_delimiter + saved_path, true)
		assert tcc_with_prod_requested_in(['-prod', '-cc', bare], project)
		assert !tcc_with_prod_requested_in(['-prod', '-cc', bare], root)
		assert os.getenv('PATH') == prefix + os.path_delimiter + saved_path
	}
	os.mkdir_all(os.join_path(project, 'bin'))!
	os.mv(os.join_path(project, bare), os.join_path(project, 'bin', bare))!
	os.setenv('PATH', 'bin' + os.path_delimiter + saved_path, true)
	assert tcc_with_prod_requested_in(['-prod', '-cc', bare], project)
	assert os.getwd() == before
}

fn test_tcc_prod_detection_prefers_launchable_local_compiler_before_path() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_pref_local_cc_${os.getpid()}')
	project := os.join_path(root, 'project')
	bin := os.join_path(root, 'bin')
	os.mkdir_all(project)!
	os.mkdir_all(bin)!
	saved_path := os.getenv('PATH')
	defer {
		os.setenv('PATH', saved_path, true)
		os.rmdir_all(root) or {}
	}
	local := os.join_path(project, 'clang')
	search := os.join_path(bin, 'clang')
	os.write_file(local, '#!/bin/sh\necho "tcc version 0.9.27"\n')!
	os.write_file(search, '#!/bin/sh\necho "clang version 19.1.7"\n')!
	os.chmod(local, 0o700)!
	os.chmod(search, 0o700)!
	os.setenv('PATH', bin + os.path_delimiter + saved_path, true)
	before := os.getwd()
	assert ccompiler_path_in('clang', project) == local
	assert tcc_with_prod_requested_in(['-prod', '-cc', 'clang'], project)
	assert !tcc_with_prod_requested_in(['-prod', '-cc', 'clang'], root)
	os.write_file(local, '#!/bin/sh\necho "clang version 19.1.7"\n')!
	os.write_file(search, '#!/bin/sh\necho "tcc version 0.9.27"\n')!
	assert !tcc_with_prod_requested_in(['-prod', '-cc', 'clang'], project)
	assert tcc_with_prod_requested_in(['-prod', '-cc', 'clang'], root)
	assert os.getwd() == before
}

fn test_ccompiler_windows_explicit_path_uses_exe_not_com_or_cmd() {
	root := os.join_path(os.vtmp_dir(), 'v_pref_windows_paths_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'clang')
	for suffix in ['', '.com', '.exe', '.cmd', '.bin'] {
		os.write_file(path + suffix, 'dummy executable-path fixture')!
		os.chmod(path + suffix, if suffix in ['.com', '.exe', '.cmd'] { 0o700 } else { 0o600 })!
	}
	assert ccompiler_windows_executable_path(path) == path + '.exe'
	assert ccompiler_windows_executable_path(path + '.bin') == path + '.bin'
	os.rm(path + '.exe')!
	assert ccompiler_windows_executable_path(path) == path
	os.rm(path)!
	assert ccompiler_windows_executable_path(path) == path
	$if windows {
		assert os.real_path(ccompiler_path_in('./clang.bin', root)) == os.real_path(path + '.bin')
	}
}

fn test_ccompiler_windows_percent_expansion_matches_launch_spelling() {
	name := 'V_PREF_COMPILER_SDK_${os.getpid()}'
	missing := 'V_PREF_COMPILER_UNSET_${os.getpid()}'
	previous := os.getenv(name)
	defer {
		if previous == '' { os.unsetenv(name) } else { os.setenv(name, previous, true) }
	}
	os.setenv(name, 'C:/compiler sdk', true)
	assert ccompiler_windows_expand_env_vars('%${name}%/clang.exe') == 'C:/compiler sdk/clang.exe'
	assert ccompiler_windows_expand_env_vars('%${missing}%/clang.exe') == '%${missing}%/clang.exe'
	assert ccompiler_windows_expand_env_vars('%%/clang.exe') == '%%/clang.exe'
	assert ccompiler_windows_expand_env_vars('%${name}/clang.exe') == '%${name}/clang.exe'
}
