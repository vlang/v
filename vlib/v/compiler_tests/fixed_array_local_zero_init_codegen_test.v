import os
import v.tempname

fn test_local_fixed_array_zero_init_declarations_use_direct_c_arrays() {
	root := os.join_path(os.vtmp_dir(), 'v3_fixed_array_local_zero_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'v3_fixed_array_local_zero')
	src := bin + '.v'
	os.write_file(src, 'module main

type Handle = voidptr

struct Empty {}

type EmptyFixed = [2]Empty

fn local_score() int {
	mut direct := [4]int{}
	direct[0] = 1
	mut wrapped := unsafe { [4]int{} }
	wrapped[1] = 2
	mut handles := unsafe { [32]Handle{} }
	handles[0] = voidptr(0)
	mut nested := unsafe { [2][3]int{} }
	nested[1][2] = 3
	empty := EmptyFixed{}
	handle_score := if handles[0] == voidptr(0) { 4 } else { 0 }
	return direct[0] + wrapped[1] + nested[1][2] + empty.len + handle_score
}

fn main() {
	println(int_str(local_score()))
}
') or {
		panic(err)
	}
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '12'
	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := generated.replace('\t', '').replace(' ', '').replace('\n', '')
	// V `int` is `i64` on 64-bit targets.
	assert compact.contains('i64direct[4]={0};')
		|| compact.contains('i64direct[4];memmove(direct,(i64[4]){0},sizeof(direct));')
		|| compact.contains('i64direct[4];memmove(direct,(i64[4]){0,0,0,0},sizeof(direct));'), generated

	assert compact.contains('i64wrapped[4]={0};') || compact.contains('i64wrapped[4]={0,0,0,0};'), generated
	assert compact.contains('void*handles[32]={0};')
		|| compact.contains('void*handles[32]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};'), generated

	assert compact.contains('i64nested[2][3]={0};')
		|| compact.contains('i64nested[2][3]={{0,0,0},{0,0,0}};'), generated
	// Check only `local_score`: platform helpers linked from vlib (such as the glibc
	// backtrace reader) may legitimately keep a fixed array on the heap.
	body_start := generated.index('local_score(void) {') or {
		panic('missing local_score definition:\n${generated}')
	}
	body_end := generated.index_after('\n}\n', body_start) or { generated.len }
	body := generated[body_start..body_end]
	assert !body.contains(' = (Array_fixed_'), body
	assert !body.contains('Array_fixed_voidptr_32 handles'), body
}

fn test_struct_field_rejects_pointer_to_fixed_array_alias() {
	root := os.join_path(os.vtmp_dir(), 'v3_fixed_array_struct_field_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'v3_fixed_array_struct_field')
	src := bin + '.v'
	os.write_file(src, 'module main

type Arr = [2]int

struct Foo {
	arr Arr
}

fn main() {
	a := Arr{}
	foo := Foo{
		arr: &a
	}
	println(foo.arr.len)
}
') or {
		panic(err)
	}
	compile := os.exec([@VEXE, '-nocache', src, '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('cannot assign to field `arr`: expected `Arr`, not `&Arr`'), compile.output

	assert !compile.output.contains('C compilation failed'), compile.output
}
