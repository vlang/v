import os

const thunk_vexe = @VEXE

fn thunk_write_project() string {
	root := os.join_path(os.vtmp_dir(), 'v3_c_callback_abi_thunk_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	// The header prototype has a `const char *` parameter and a C `int` result,
	// neither of which the `fn C.` declaration below spells the same way.
	header := '#ifndef V3_NATIVE_THUNK_H
#define V3_NATIVE_THUNK_H
static inline int native_run(int (*cb)(const char *name, void *arg), void *arg) {
	return cb("entry", arg);
}
#endif
'
	os.write_file(os.join_path(root, 'native_thunk.h'), header) or { panic(err) }
	src := 'module main

#include "@DIR/native_thunk.h"

fn C.native_run(cb fn (&char, voidptr) int, arg voidptr) int

fn on_entry(name &char, arg voidptr) int {
	_ = arg
	return unsafe { cstring_to_vstring(name) }.len
}

fn main() {
	println(C.native_run(on_entry, unsafe { nil }))
}
'
	path := os.join_path(root, 'main.v')
	os.write_file(path, src) or { panic(err) }
	return path
}

fn test_c_callback_abi_thunk_defers_to_the_header_prototype() {
	src := thunk_write_project()
	defer {
		os.rmdir_all(os.dir(src)) or {}
	}
	c_file := os.join_path(os.dir(src), 'main.c')
	gen := os.exec([thunk_vexe, '-o', c_file, '${src}'])
	assert gen.exit_code == 0, gen.output
	c_source := os.read_file(c_file) or { panic(err) }
	// The ABI thunk converts V's `int` result to C `int`; it is passed as `void*`
	// so the header's `const char *` parameter type decides the conversion.
	assert c_source.contains('native_run((void*)'), c_source
	// clang 16+ rejects mismatched function pointers by default on Linux.
	if os.exists_in_system_path('clang') {
		exe := os.join_path(os.dir(src), 'main_clang')
		run := os.exec([thunk_vexe, '-cc', 'clang', '-o', exe, 'run', '${src}'])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '5'
	}
}
