import os
import v.tempname

// Slicing a fixed array must evaluate the slice bounds exactly once, so
// side-effecting bound expressions are not run multiple times in the generated C.
fn test_fixed_array_slice_evaluates_bounds_once() {
	root := os.join_path(os.vtmp_dir(), 'v3_fixed_slice_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }

	src := '
fn start_idx() int {
	println("eval")
	return 1
}

fn main() {
	mut a := [3]int{}
	s := a[start_idx()..3]
	println(s.len)
}
'
	src_file := os.join_path(root, 'v3_fixed_slice.v')
	os.write_file(src_file, src) or { panic(err) }
	bin := os.join_path(root, 'v3_fixed_slice_bin')
	compile := os.exec([@VEXE, src_file, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.count('eval') == 1, run.output

	c_file := os.join_path(root, 'v3_fixed_slice.c')
	generate_c := os.exec([@VEXE, '-o', c_file, src_file])
	assert generate_c.exit_code == 0, generate_c.output
	generated_c := os.read_file(c_file) or { panic(err) }
	assert !generated_c.contains('(int[3]){a[0], a[1], a[2]}'), generated_c
}
