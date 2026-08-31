import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// Slicing a fixed array must evaluate the slice bounds exactly once, so
// side-effecting bound expressions are not run multiple times in the generated C.
fn test_fixed_array_slice_evaluates_bounds_once() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fixed_slice_test')
	build := os.execute('${vexe} -gc none -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

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
	src_file := os.join_path(os.temp_dir(), 'v3_fixed_slice.v')
	os.write_file(src_file, src) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_fixed_slice_bin')
	os.rm(bin) or {}
	compile := os.execute('${v3_bin} ${src_file} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.count('eval') == 1, run.output

	c_file := os.join_path(os.temp_dir(), 'v3_fixed_slice.c')
	generate_c := os.execute('${v3_bin} -o ${c_file} ${src_file}')
	assert generate_c.exit_code == 0, generate_c.output
	generated_c := os.read_file(c_file) or { panic(err) }
	assert !generated_c.contains('(int[3]){a[0], a[1], a[2]}'), generated_c
}
