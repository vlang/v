import os

const gcg_vexe = @VEXE
const gcg_tests_dir = os.dir(@FILE)
const gcg_v3_dir = os.dir(gcg_tests_dir)
const gcg_vlib_dir = os.dir(gcg_v3_dir)
const gcg_v3_src = os.join_path(gcg_v3_dir, 'v3.v')

// A `const` whose initializer is a generic call (`stdatomic.new_atomic(0)`)
// keeps the generic return type `&AtomicVal[T]`. The backing global variable
// must be declared with the concrete monomorphized type recovered from the
// (already specialized) initializer, not the undeclared `AtomicVal_T`.
fn test_generic_const_global_uses_concrete_storage_type() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_const_${pid}')
	src := os.join_path(os.temp_dir(), 'v3_generic_const_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_generic_const_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	build :=
		os.execute('${gcg_vexe} -gc none -d ownership -path "${gcg_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${gcg_v3_src}')
	assert build.exit_code == 0, build.output
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
	compile := os.execute('${v3_bin} ${src} -d ownership -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
}
