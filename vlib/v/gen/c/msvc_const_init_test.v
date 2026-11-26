module main

import os
import v.cmdexec

fn testsuite_begin() {
	// The MSVC CI jobs put `-cc msvc` in VFLAGS; each generation here picks its compiler.
	os.unsetenv('VFLAGS')
}

// MSVC reads the GNU escape `'\e'` as `'e'`, and rejects a string's compound literal as a
// file-scope initializer, so a `$d` string constant is assigned when the program starts.
fn test_msvc_consts_avoid_gnu_escapes_and_static_string_literals() {
	root := os.join_path(os.vtmp_dir(), 'cgen_msvc_const_init_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, "const esc = `\\e`
const from_define = \$d('cgen_msvc_const_init', 'three')

fn main() {
	println(int(esc))
	println(from_define)
}
") or {
		panic(err)
	}
	c_file := os.join_path(root, 'msvc.c')
	res := cmdexec.run(@VEXE, ['-gc', 'none', '-os', 'windows', '-cc', 'msvc', '-o', c_file, source])
	assert res.exit_code == 0, res.output
	code := os.read_file(c_file) or { panic(err) }
	assert !code.contains("'\\e'")
	assert code.contains('#define main__esc (27)')
	assert code.contains('string main__from_define;')
	assert !code.contains('string main__from_define =')
	assert code.contains('main__from_define = ')
}
