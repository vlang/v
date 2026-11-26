import os
import v.tempname

fn test_addressed_slice_rvalue_lives_through_clone_call() {
	dir := os.join_path(os.vtmp_dir(), 'v3_rvalue_pointer_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	src := os.join_path(dir, 'v3_rvalue_pointer.v')
	out := os.join_path(dir, 'program')
	os.write_file(src, "module main

fn cloned_tail(bytes []u8) (int, []u8) {
	return 1, bytes[1..].clone()
}

fn main() {
	_, tail := cloned_tail([u8(1), 2, 3])
	assert tail == [u8(2), 3]
	println('ok')
}
") or {
		panic(err)
	}
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', out])
	assert compile.exit_code == 0, compile.output
	c_source := os.read_file(out + '.c') or { panic(err) }
	assert !c_source.contains('; &_t'), c_source
	run := os.exec([out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
