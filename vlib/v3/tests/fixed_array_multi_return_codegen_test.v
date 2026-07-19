import os

const fixed_array_multi_vexe = @VEXE
const fixed_array_multi_tests_dir = os.dir(@FILE)
const fixed_array_multi_v3_dir = os.dir(fixed_array_multi_tests_dir)
const fixed_array_multi_vlib_dir = os.dir(fixed_array_multi_v3_dir)
const fixed_array_multi_v3_src = os.join_path(fixed_array_multi_v3_dir, 'v3.v')

fn fixed_array_multi_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fixed_array_multi_return_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${fixed_array_multi_vexe} -gc none -path "${fixed_array_multi_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${fixed_array_multi_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn compact_c_whitespace(s string) string {
	return s.replace('\t', '').replace(' ', '').replace('\n', '')
}

fn has_fixed_array_memmove_copy(compact string, name string) bool {
	start := compact.index('memmove(${name},') or { return false }
	end := compact[start..].index(');') or { return false }
	fragment := compact[start..start + end]
	return fragment.contains('sizeof(${name})')
}

fn has_fixed_array_assignment(compact string, typ string, name string) bool {
	return compact.contains('${typ}${name}=')
}

fn has_fixed_array_memmove_source(compact string, name string) bool {
	return compact.contains(',${name},sizeof(')
}

fn test_fixed_array_multi_return_payloads_use_c_arrays_and_memmove() {
	v3_bin := fixed_array_multi_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_fixed_array_multi_return_${os.getpid()}.v')
	os.write_file(src, 'module main

fn pair() (int, [3]int) {
	mut xs := [3]int{}
	xs[0] = 1
	xs[1] = 2
	xs[2] = 3
	return 7, xs
}

fn literal_pair() ([3]int, int) {
	mut zs := [3]int{}
	zs[0] = 4
	zs[1] = 5
	zs[2] = 6
	return zs, 9
}

fn defer_pair() (int, [2]int) {
	defer {
		_ := 0
	}
	mut ds := [2]int{}
	ds[0] = 8
	ds[1] = 9
	return 11, ds
}

fn choose_pair(c bool) (int, [3]int) {
	mut choice := [3]int{}
	choice[0] = 10
	choice[1] = 11
	choice[2] = 12
	return if c {
		13
		choice
	} else {
		17
		choice
	}
}

fn main() {
	n, xs := pair()
	assert n == 7
	assert xs[0] == 1
	assert xs[2] == 3
	mut m := 0
	mut ys := [3]int{}
	m, ys = pair()
	assert m == 7
	assert ys[1] == 2
	zs, q := literal_pair()
	assert q == 9
	assert zs[2] == 6
	d, ds := defer_pair()
	assert d == 11
	assert ds[1] == 9
	choice_n, choice := choose_pair(false)
	assert choice_n == 17
	assert choice[0] == 10
	assert choice[2] == 12
	println(int_str(n + xs[1] + m + ys[2] + q + zs[0] + d + ds[1] + choice_n + choice[2]))
}
') or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_fixed_array_multi_return_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '81'
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('int xs[3];'), generated
	assert generated.contains('int ys[3] = {0};'), generated
	assert generated.contains('int zs[3];'), generated
	assert generated.contains('int ds[2];'), generated
	assert generated.contains('int choice[3];'), generated
	compact := compact_c_whitespace(generated)
	assert has_fixed_array_memmove_copy(compact, 'xs'), generated
	assert has_fixed_array_memmove_copy(compact, 'ys'), generated
	assert has_fixed_array_memmove_copy(compact, 'zs'), generated
	assert has_fixed_array_memmove_copy(compact, 'ds'), generated
	assert has_fixed_array_memmove_copy(compact, 'choice'), generated
	assert has_fixed_array_memmove_source(compact, 'choice'), generated
	assert !has_fixed_array_assignment(compact, 'Array_fixed_int_3', 'xs'), generated
	assert !has_fixed_array_assignment(compact, 'Array_fixed_int_3', 'ys'), generated
	assert !has_fixed_array_assignment(compact, 'Array_fixed_int_3', 'zs'), generated
	assert !has_fixed_array_assignment(compact, 'Array_fixed_int_2', 'ds'), generated
	assert !generated.contains('return (multi_return_int_Array_fixed_int_3){7, xs};'), generated
	assert !compact.contains('.arg1=choice'), generated
}

fn test_deferred_multi_return_pointer_value_slot_uses_expected_type() {
	v3_bin := fixed_array_multi_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_deferred_multi_return_pointer_value_${os.getpid()}.v')
	os.write_file(src, 'module main

struct S {
	value int
}

fn f() (S, int) {
	defer {
		_ := 0
	}
	s := S{
		value: 41
	}
	return &s, 1
}

fn main() {
	s, n := f()
	println(int_str(s.value + n))
}
') or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_deferred_multi_return_pointer_value_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := compact_c_whitespace(generated)
	assert compact.contains('{*&s,1}') || compact.contains('{*(&s),1}'), generated
	assert !compact.contains('{&s,1}'), generated
}
