module fastc

import os
import v.cmdexec
import v.flat
import v.pref

fn test_fastc_explicit_pthread_helpers_keep_the_native_header_ownership() {
	$if !macos && !linux {
		return
	}
	prefs := pref.new_preferences()
	mut functions := map[string]FastcFunctionSignature{}
	collect_function_signatures('module main
@[c_extern]
fn C.v3_pthread_zero() C.pthread_t
@[c_extern]
fn C.v3_pthread_create(worker &C.pthread_t, stack_size uint, start_routine fn (voidptr) voidptr, arg voidptr) int
@[c_extern]
fn C.v3_pthread_is_current(worker C.pthread_t) int
@[c_extern]
fn C.unrelated_native_hook(value voidptr) int
', 'pthread_c_extern.c.v', FastcSourceHeader{ module_name: 'main' }, prefs, []int{},
		map[string]bool{}, flat.TypeTextBindings{}, map[string]bool{}, mut functions) or {
		panic(err)
	}
	declarations := fastc_c_extern_prototypes(functions)
	// Without the native header, explicit declarations must still be available.
	for name in ['v3_pthread_zero', 'v3_pthread_create', 'v3_pthread_is_current',
		'unrelated_native_hook'] {
		assert declarations.contains(' ${name}('), declarations
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_pthread_extern_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'prototype.c')
	helper := os.join_path(prefs.vroot, 'vlib', 'v', 'pthread_helper.h')
	os.write_file(c_file, '#include "${helper}"\ntypedef size_t usize;\ntypedef void* voidptr;\n' +
		declarations) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compiled := cmdexec.run(tcc, ['-std=gnu11', '-c', '-o', os.join_path(root, 'prototype.o'),
		c_file])
	assert compiled.exit_code == 0, compiled.output
	with_header := cmdexec.run(tcc, ['-E', c_file])
	assert with_header.exit_code == 0, with_header.output
	assert !with_header.output.contains('extern int v3_pthread_create('), with_header.output
	assert with_header.output.contains('extern int unrelated_native_hook(voidptr);'), with_header.output
	// The same declarations stay active in translation units without this header.
	headerless_file := os.join_path(root, 'headerless.c')
	os.write_file(headerless_file, 'typedef void* pthread_t;\ntypedef unsigned long usize;\ntypedef void* voidptr;\n' +
		declarations) or { panic(err) }
	without_header := cmdexec.run(tcc, ['-E', headerless_file])
	assert without_header.exit_code == 0, without_header.output
	for name in ['v3_pthread_zero', 'v3_pthread_create', 'v3_pthread_is_current',
		'unrelated_native_hook'] {
		assert without_header.output.contains(' ${name}('), without_header.output
	}
}

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
', 'const_c_extern.v', header, prefs, []int{}, map[string]bool{}, flat.TypeTextBindings{},
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
