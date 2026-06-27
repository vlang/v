import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn mut_param_reassign_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_mut_param_reassign_codegen_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn mut_param_reassign_run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn mut_param_reassign_run_bad(v3_bin string, name string, source string, expected string) {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains(expected), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn test_mut_array_param_reassigns_to_base_type() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'mut_array_param_reassign', 'fn replace(mut xs []int) {
	mut tmp := []int{}
	tmp << 4
	tmp << 9
	xs = tmp.clone()
}

fn main() {
	mut xs := []int{}
	xs << 1
	replace(mut xs)
	assert xs.len == 2
	assert xs[0] == 4
	assert xs[1] == 9
	println("ok")
}
')
	assert out == 'ok'
}

fn test_generic_mut_array_param_reassigns_to_base_type() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'generic_mut_array_param_reassign', 'struct Item {
	data int
	priority int
}

fn replace[T](mut xs []T, value T) {
	mut tmp := []T{}
	tmp << value
	xs = tmp.clone()
}

fn main() {
	mut xs := []Item{}
	replace(mut xs, Item{data: 7, priority: 3})
	assert xs.len == 1
	assert xs[0].data == 7
	assert xs[0].priority == 3
	println("ok")
}
')
	assert out == 'ok'
}

fn test_mut_map_param_reassigns_to_base_type() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'mut_map_param_reassign', "fn replace(mut m map[string]int) {
	tmp := {
		'answer': 42
	}
	m = tmp.clone()
}

fn main() {
	mut m := map[string]int{}
	m['old'] = 1
	replace(mut m)
	assert m.len == 1
	assert m['answer'] == 42
	println('ok')
}
")
	assert out == 'ok'
}

fn test_mut_param_reassign_keeps_invalid_assignments_rejected() {
	v3_bin := mut_param_reassign_build_v3()
	mut_param_reassign_run_bad(v3_bin, 'bad_mut_array_param_reassign_elem', "fn bad(mut xs []int) {
	mut ys := []string{}
	ys << 'bad'
	xs = ys
}

fn main() {
	mut xs := []int{}
	bad(mut xs)
}
",
		'cannot assign `[]string` to `[]int`')
	mut_param_reassign_run_bad(v3_bin, 'bad_mut_array_param_reassign_scalar', 'fn bad(mut xs []int) {
	xs = 1
}

fn main() {
	mut xs := []int{}
	bad(mut xs)
}
',
		'cannot assign `int` to `[]int`')
	mut_param_reassign_run_bad(v3_bin, 'bad_pointer_local_reassign_value', 'fn main() {
	mut xs := []int{}
	mut p := &xs
	mut tmp := []int{}
	p = tmp
}
',
		'cannot assign `[]int` to `&[]int`')
}
