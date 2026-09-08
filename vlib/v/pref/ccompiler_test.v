module pref

import os

fn test_ccompiler_can_assemble_rejects_tcc_alias() {
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
	found := find_system_assembler() or { panic('failed to find fake Clang assembler') }
	assert found == alias_path
}
