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

fn test_mut_string_param_concat_reads_as_string() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'mut_string_param_concat', "fn add(mut s string) string {
	return s + '!'
}

fn main() {
	mut s := 'hi'
	println(add(mut s))
}
")
	assert out == 'hi!'
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

fn test_multi_return_assign_to_mut_array_param_uses_base_type() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'mut_array_param_multi_return_reassign', 'fn pair() ([]int, int) {
	return [1, 2], 7
}

fn replace(mut xs []int) {
	xs, _ = pair()
}

fn main() {
	mut xs := [0]
	replace(mut xs)
	assert xs.len == 2
	assert xs[0] == 1
	assert xs[1] == 2
	println("ok")
}
')
	assert out == 'ok'
}

fn test_inner_scope_redefinition_of_mut_param_is_rejected_before_c_compile() {
	v3_bin := mut_param_reassign_build_v3()
	mut_param_reassign_run_bad(v3_bin, 'bad_mut_array_param_inner_scope_redefinition', 'fn pair() ([]int, int) {
	return [1], 0
}

fn replace(mut xs []int) {
	if true {
		mut xs := 0
		_ = xs
	}
	if true {
		xs, _ = pair()
	}
}

fn main() {
	mut xs := [0]
	replace(mut xs)
	assert xs.len == 1
	assert xs[0] == 1
	println("ok")
}
',
		'redefinition of xs')
}

fn test_pointer_local_redefinition_of_mut_param_is_rejected_before_c_compile() {
	v3_bin := mut_param_reassign_build_v3()
	mut_param_reassign_run_bad(v3_bin, 'bad_mut_array_param_pointer_redefinition', 'fn f(mut xs []int) {
	mut local := []int{}
	mut other := []int{}
	{
		xs := &local
		xs = &other
		_ = xs
	}
}

fn main() {
	mut xs := [0]
	f(mut xs)
	assert xs.len == 1
	assert xs[0] == 0
	println("ok")
}
',
		'redefinition of xs')
}

fn test_mut_param_compound_assign_and_postfix_store_through_pointer() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'mut_param_compound_assign', 'fn inc(mut n int) {
	n += 1
	n++
}

fn main() {
	mut x := 1
	inc(mut x)
	assert x == 3
	println("ok")
}
')
	assert out == 'ok'
}

fn test_mut_param_unsigned_right_shift_assign_stores_through_pointer() {
	v3_bin := mut_param_reassign_build_v3()
	out := mut_param_reassign_run_good(v3_bin, 'mut_param_unsigned_right_shift_assign', 'fn shift(mut n int) {
	n >>>= 1
}

fn main() {
	mut x := 8
	shift(mut x)
	assert x == 4
	println("ok")
}
')
	assert out == 'ok'
}

fn test_mut_param_reassign_keeps_invalid_assignments_rejected() {
	v3_bin := mut_param_reassign_build_v3()
	mut_param_reassign_run_bad(v3_bin, 'bad_same_scope_mut_string_param_redeclare', "fn shadow_read(mut s string) string {
	mut s := 'local'
	return s + '!'
}

fn main() {
	mut s := 'param'
	_ = shadow_read(mut s)
}
",
		'redefinition of s')
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
	mut_param_reassign_run_bad(v3_bin, 'bad_shadowed_mut_param_multi_return', 'fn pair() ([]int, int) {
	mut xs := []int{}
	return xs, 7
}

fn replace(mut xs []int) {
	if true {
		mut xs := 0
		xs, _ = pair()
	}
}

fn main() {
	mut xs := []int{}
	replace(mut xs)
}
',
		'redefinition of xs')
}
