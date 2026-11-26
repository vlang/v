import os
import v.tempname

fn test_mut_array_param_clone_reassigns_value_array() {
	dir := os.join_path(os.vtmp_dir(), 'v3_mut_param_array_clone_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_mut_param_array_clone_input')
	src := bin + '.v'
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

	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('cannot assign `&[]int` to `[]int`'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_mut_array_param_assignment_still_rejects_non_array() {
	dir := os.join_path(os.vtmp_dir(), 'v3_mut_param_array_clone_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	bin := os.join_path(dir, 'v3_mut_array_param_bad_assign')
	src := bin + '.v'
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

	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot assign'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}
