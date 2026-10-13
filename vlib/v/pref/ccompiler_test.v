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
