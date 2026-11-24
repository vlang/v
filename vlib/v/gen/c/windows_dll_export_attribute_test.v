module main

import os
import v.cmdexec

fn testsuite_begin() {
	// The Windows CI jobs put `-cc msvc` in VFLAGS; each generation here picks its compiler.
	os.unsetenv('VFLAGS')
}

// TinyCC exports only `__declspec(dllexport)` functions from a Windows DLL, so an
// `@[export]` function of a TinyCC-built DLL was missing and `dl.sym` returned nil.
// MinGW's linker exports every symbol of a DLL that marks none, so GCC keeps the
// visibility attribute and its DLLs keep exporting all functions.
fn test_windows_dll_export_attribute_follows_the_c_compiler() {
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
	dllexport := '__declspec(dllexport) int answer(void)'
	visibility := '__attribute__((visibility("default"))) int answer(void)'
	// Another host builds a Windows amd64 program with the MinGW-w64 GCC instead of TinyCC
	// (see v3_windows_cross_c_compiler), so only a Windows host generates TinyCC's attribute.
	tcc_attribute := $if windows { dllexport } $else { visibility }
	for cc, attribute in {
		'tcc':  tcc_attribute
		'gcc':  visibility
		'msvc': dllexport
	} {
		c_file := os.join_path(root, 'lib_${cc}.c')
		res := cmdexec.run(@VEXE, ['-os', 'windows', '-arch', 'amd64', '-cc', cc, '-shared', '-o',
			c_file, source])
		assert res.exit_code == 0, '${cc}: ${res.output}'
		code := os.read_file(c_file) or { panic(err) }
		assert code.contains(attribute), cc
	}
}
