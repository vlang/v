import os
import v.tempname

fn test_direct_filelock_c_helpers_emit_inline_runtime_defs() {
	root := os.join_path(os.vtmp_dir(), 'v3_filelock_helpers_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_filelock_helpers_input.v')
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

	bin := os.join_path(root, 'v3_filelock_helpers_input')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert !c_code.contains('filelock_helpers.h'), c_code
	assert c_code.contains('\nstatic inline int v_filelock_lock('), c_code
	assert c_code.contains('\nstatic inline int v_filelock_unlock('), c_code
	assert !c_code.contains('\ni32 v_filelock_lock('), c_code
	assert !c_code.contains('\ni32 v_filelock_unlock('), c_code

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '47'

	method_src := os.join_path(root, 'v3_filelock_helpers_method_input.v')
	os.write_file(method_src, 'module main

import os.filelock

fn use(mut file_lock filelock.FileLock) bool {
	if file_lock.try_acquire() {
		return file_lock.release()
	}
	return false
}

fn main() {
	mut file_lock := filelock.new("v3_filelock_helpers_method_input.lock")
	println(use(mut file_lock))
}
	') or {
		panic(err)
	}
	method_bin := os.join_path(root, 'v3_filelock_helpers_method_input')
	method_compile := os.exec([@VEXE, '${method_src}', '-b', 'c', '-o', method_bin])
	assert method_compile.exit_code == 0, method_compile.output
	assert !method_compile.output.contains('C compilation failed'), method_compile.output
	method_c_code := os.read_file(method_bin + '.c') or { panic(err) }
	assert method_c_code.contains('\nstatic inline int v_filelock_lock('), method_c_code
	assert method_c_code.contains('\nstatic inline int v_filelock_unlock('), method_c_code
}
