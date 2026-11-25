// vtest ownership
import os
import v.cmdexec
import v.tempname

// A `const` whose initializer is a generic call (`stdatomic.new_atomic(0)`)
// keeps the generic return type `&AtomicVal[T]`. The backing global variable
// must be declared with the concrete monomorphized type recovered from the
// (already specialized) initializer, not the undeclared `AtomicVal_T`.
fn test_generic_const_global_uses_concrete_storage_type() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_generic_const_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_generic_const.v')
	out := os.join_path(root, 'v3_generic_const_program')
	os.write_file(src, 'module main

import sync.stdatomic

const g_state = stdatomic.new_atomic(0)

fn g_state_ref() &stdatomic.AtomicVal[int] {
	return g_state
}

fn main() {
	g_state_ref().store(7)
	assert g_state_ref().load() == 7
}
') or {
		panic(err)
	}
	compile := cmdexec.run_in_merged_with_env(compiler, [src, '-d', 'ownership', '-b', 'c', '-o',
		out], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
}
