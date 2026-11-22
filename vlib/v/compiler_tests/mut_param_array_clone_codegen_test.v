import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v.v')

// One compiler serves the whole suite; each case used to build its own.
const build_v3_path = os.join_path(os.temp_dir(), 'v3_mut_param_array_clone_test_${os.getpid()}')

fn testsuite_begin() {
	v3_bin := build_v3_path
	build :=
		os.exec([vexe, '-gc', 'none', '-path', '${vlib_dir}' + '|@vlib|@vmodules', '-o', v3_bin,
			'${v3_src}'])
	assert build.exit_code == 0, build.output
}

fn testsuite_end() {
	os.rm(build_v3_path) or {}
}

fn build_v3() string {
	return build_v3_path
}

fn test_mut_array_param_clone_reassigns_value_array() {
	v3_bin := build_v3()

	src := os.join_path(os.temp_dir(), 'v3_mut_param_array_clone_input_${os.getpid()}.v')
	os.write_file(src, "fn update(mut xs []int) {
	mut ys := xs.clone()
	ys[0] = 2
	xs = ys.clone()
}

fn main() {
	mut xs := [1, 1]
	update(mut xs)
	assert xs[0] == 2
	assert xs[1] == 1
	println('ok')
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_mut_param_array_clone_input_${os.getpid()}')
	compile := os.exec([v3_bin, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('cannot assign `&[]int` to `[]int`'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_mut_array_param_assignment_still_rejects_non_array() {
	v3_bin := build_v3()

	src := os.join_path(os.temp_dir(), 'v3_mut_array_param_bad_assign_${os.getpid()}.v')
	os.write_file(src, 'fn update(mut xs []int) {
	xs = 1
}

fn main() {
	mut xs := [1, 2]
	update(mut xs)
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_mut_array_param_bad_assign_${os.getpid()}')
	compile := os.exec([v3_bin, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot assign'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}
