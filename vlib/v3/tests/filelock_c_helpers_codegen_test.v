import os

const filelock_helpers_vexe = @VEXE
const filelock_helpers_tests_dir = os.dir(@FILE)
const filelock_helpers_v3_dir = os.dir(filelock_helpers_tests_dir)
const filelock_helpers_vlib_dir = os.dir(filelock_helpers_v3_dir)
const filelock_helpers_v3_src = os.join_path(filelock_helpers_v3_dir, 'v3.v')

fn filelock_helpers_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_filelock_helpers_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${filelock_helpers_vexe} -gc none -path "${filelock_helpers_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${filelock_helpers_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_direct_filelock_c_helpers_emit_inline_runtime_defs() {
	v3_bin := filelock_helpers_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_filelock_helpers_input_${os.getpid()}.v')
	os.write_file(src, 'module main

fn C.v_filelock_lock(i32, i32, i32, u64, u64) int
fn C.v_filelock_unlock(i32, u64, u64) int

fn filelock_score() int {
	lock_result := C.v_filelock_lock(i32(-1), 1, 1, u64(0), u64(0))
	unlock_result := C.v_filelock_unlock(i32(-1), u64(0), u64(0))
	if lock_result != 0 && unlock_result != 0 {
		return 47
	}
	return 0
}

fn main() {
	println(filelock_score())
}
	') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_filelock_helpers_input_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert !c_code.contains('filelock_helpers.h'), c_code
	assert c_code.contains('\nstatic inline int v_filelock_lock('), c_code
	assert c_code.contains('\nstatic inline int v_filelock_unlock('), c_code
	assert !c_code.contains('\ni32 v_filelock_lock('), c_code
	assert !c_code.contains('\ni32 v_filelock_unlock('), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '47'
}
