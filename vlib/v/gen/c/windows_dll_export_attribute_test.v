module main

import os
import v.cmdexec

fn testsuite_begin() {
	// The Windows CI jobs put `-cc msvc` in VFLAGS; each generation here picks its compiler.
	os.unsetenv('VFLAGS')
}

// A V export is visible in generated C regardless of the Windows toolchain.
fn test_windows_dll_export_attribute_for_each_c_compiler() {
	root := os.join_path(os.vtmp_dir(), 'windows_dll_export_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'lib.v')
	os.write_file(source, 'module lib

@[export: "answer"]
pub fn answer() int {
	return 42
}
') or { panic(err) }
	dllexport := '__declspec(dllexport) int (answer)(void)'
	for cc in ['tcc', 'gcc', 'msvc'] {
		c_file := os.join_path(root, 'lib_${cc}.c')
		res := cmdexec.run(@VEXE, ['-os', 'windows', '-arch', 'amd64', '-cc', cc, '-shared', '-o',
			c_file, source])
		assert res.exit_code == 0, '${cc}: ${res.output}'
		code := os.read_file(c_file) or { panic(err) }
		assert code.contains(dllexport), cc
	}
}
