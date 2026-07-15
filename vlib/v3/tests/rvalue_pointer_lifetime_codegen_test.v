import os

const rvalue_pointer_vexe = @VEXE
const rvalue_pointer_tests_dir = os.dir(@FILE)
const rvalue_pointer_v3_dir = os.dir(rvalue_pointer_tests_dir)
const rvalue_pointer_vlib_dir = os.dir(rvalue_pointer_v3_dir)
const rvalue_pointer_v3_src = os.join_path(rvalue_pointer_v3_dir, 'v3.v')

fn test_addressed_slice_rvalue_lives_through_clone_call() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_rvalue_pointer_${pid}')
	src := os.join_path(os.temp_dir(), 'v3_rvalue_pointer_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_rvalue_pointer_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	build :=
		os.execute('${rvalue_pointer_vexe} -gc none -path "${rvalue_pointer_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${rvalue_pointer_v3_src}')
	assert build.exit_code == 0, build.output
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
	compile := os.execute('${v3_bin} ${src} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	c_source := os.read_file(out + '.c') or { panic(err) }
	assert !c_source.contains('; &_t'), c_source
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
