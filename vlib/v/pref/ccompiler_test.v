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
