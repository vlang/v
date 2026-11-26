import os
import v.tempname

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

fn test_multi_return_uses_fixed_array_copies_and_dereferences_pointer_values() {
	root := os.join_path(os.vtmp_dir(), 'v3_fixed_array_multi_return_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'v3_fixed_array_multi_return')
	src := bin + '.v'
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

struct S {
	value int
}

fn deferred_pointer_pair() (S, int) {
	defer {
		_ := 0
	}
	s := S{
		value: 41
	}
	return &s, 1
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
	s, pointer_n := deferred_pointer_pair()
	println(int_str(s.value + pointer_n))
}
') or {
		panic(err)
	}
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ['81', '42'], run.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := compact_c_whitespace(generated)
	assert generated.contains('i64 xs[3];'), generated
	assert compact.contains('i64ys[3]={0};')
		|| compact.contains('i64ys[3];memmove(ys,(i64[3]){0},sizeof(ys));')
		|| compact.contains('i64ys[3];memmove(ys,(i64[3]){0,0,0},sizeof(ys));'), generated
	assert generated.contains('i64 zs[3];'), generated
	assert generated.contains('i64 ds[2];'), generated
	assert generated.contains('i64 choice[3];'), generated
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

	pointer_body := generated.all_after(' deferred_pointer_pair(void) {').all_before('\n}')
	assert pointer_body.len > 0, generated
	pointer_c := compact_c_whitespace(pointer_body)
	assert pointer_c.contains('{*&s,1}') || pointer_c.contains('{*(&s),1}'), pointer_body
	assert !pointer_c.contains('{&s,1}'), pointer_body
}
