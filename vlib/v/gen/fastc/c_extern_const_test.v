module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_c_extern_preserves_const_parameter_names() {
	prefs := pref.new_preferences()
	header := FastcSourceHeader{
		module_name: 'main'
	}
	mut functions := map[string]FastcFunctionSignature{}
	collect_function_signatures('module main
@[c_extern]
fn C.renameat(old_directory i32, const_old_path &char, new_directory i32, const_new_path &char) i32
@[c_extern]
fn C.const_pair(const_left &char, right &char)
', 'const_c_extern.v', header, prefs, []int{}, map[string]bool{}, map[string]string{},
		map[string]bool{}, mut functions) or { panic(err) }
	declarations := fastc_c_extern_prototypes(functions)
	assert declarations.contains('extern i32 renameat(i32, const char*, i32, const char*);'), declarations
	assert declarations.contains('extern void const_pair(const char*, char*);'), declarations
	root := os.join_path(os.vtmp_dir(), 'fastc_const_extern_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	// The emitted prototype must agree with the system header's const pointer ABI.
	c_file := os.join_path(root, 'prototype.c')
	os.write_file(c_file, '#include <stdint.h>\n#include <stdio.h>\ntypedef int32_t i32;\n' +
		declarations) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compiled := cmdexec.run(tcc, ['-std=gnu11', '-c', '-o', os.join_path(root, 'prototype.o'),
		c_file])
	assert compiled.exit_code == 0, compiled.output
}

fn test_fastc_declares_functions_from_matching_native_inputs() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_native_prototype_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'helper.c'), 'int fastc_native_answer(void) { return 42; }\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'helper_nix.c.v'), 'module main
#flag @DIR/helper.c
fn C.fastc_native_answer() int
fn answer() int { return C.fastc_native_answer() }
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main
fn main() { println(answer()) }
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42', run.output
}
