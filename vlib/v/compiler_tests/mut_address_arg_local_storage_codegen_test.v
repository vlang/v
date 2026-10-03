import os

// A call that returns scalars cannot return the address it was given, however many
// scalars, and whether or not they come in an Option: `n, err := read(mut &buf)` followed
// by `return n, err` leaves `buf` on the stack. One that returns a pointer among them
// still moves it, and so does one that returns a Result, whose error may keep the
// address; `mut &buf` is then the pointer the local is stored as.
fn test_mut_address_arg_keeps_scalar_result_locals_on_the_stack() {
	root := os.join_path(os.vtmp_dir(), 'v3_mut_address_arg_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_path := os.join_path(root, 'main.v')
	source := 'module main

struct Entry {
mut:
	ino  u64
	name [16]u8
}

fn fill(mut e Entry) (u64, u64) {
	e.ino = 7
	return 0, 0
}

fn fill_result(mut e Entry) !u64 {
	e.ino = 11
	return 1
}

fn fill_option(mut e Entry) ?u64 {
	e.ino = 13
	return 2
}

fn fill_option_pair(mut e Entry) ?(u64, u64) {
	e.ino = 19
	return 5, 6
}

fn fill_result_pair(mut e Entry) !(u64, u64) {
	e.ino = 17
	return 3, 4
}

fn fill_and_keep(mut e Entry) (u64, &Entry) {
	e.ino = 9
	return 0, unsafe { e }
}

fn scalars() (u64, u64) {
	mut on_stack := Entry{}
	ret, err := fill(mut &on_stack)
	if err != 0 {
		return ret, err
	}
	return on_stack.ino, 0
}

fn forwarded() (u64, u64) {
	mut forwarded_on_stack := Entry{}
	return fill(mut &forwarded_on_stack)
}

fn result() !u64 {
	mut result_on_heap := Entry{}
	n := fill_result(mut &result_on_heap)!
	return n
}

fn forwarded_result() !u64 {
	mut forwarded_result_on_heap := Entry{}
	return fill_result(mut &forwarded_result_on_heap)
}

fn option() ?u64 {
	mut option_on_stack := Entry{}
	n := fill_option(mut &option_on_stack) or { return none }
	return n
}

fn forwarded_option() ?u64 {
	mut forwarded_option_on_stack := Entry{}
	return fill_option(mut &forwarded_option_on_stack)
}

fn option_pair() ?(u64, u64) {
	mut option_pair_on_stack := Entry{}
	a, b := fill_option_pair(mut &option_pair_on_stack)?
	return a, b
}

fn result_pair() !(u64, u64) {
	mut pair_on_heap := Entry{}
	a, b := fill_result_pair(mut &pair_on_heap)!
	return a, b
}

fn pointer() (u64, &Entry) {
	mut on_heap := Entry{}
	ret, kept := fill_and_keep(mut &on_heap)
	return ret, kept
}

fn main() {
	a, _ := scalars()
	b, _ := forwarded()
	c := result() or { 0 }
	d := forwarded_result() or { 0 }
	e := option() or { 0 }
	f, _ := result_pair() or { 0, 0 }
	g := forwarded_option() or { 0 }
	h, _ := option_pair() or { u64(0), u64(0) }
	_, kept := pointer()
	println(a + b + c + d + e + f + g + h + kept.ino)
}
'
	os.write_file(main_path, source) or { panic(err) }
	out_path := os.join_path(root, 'out.c')
	result := os.exec([@VEXE, '-new-compiler', '-gc', 'none', '-nocache', '-warn-about-allocs',
		'-o', out_path, main_path])
	assert result.exit_code == 0, result.output
	moved := 'allocation (local moved to the heap: its address escapes)'
	on_heap := ['result_on_heap', 'forwarded_result_on_heap', 'pair_on_heap', 'on_heap']
	assert result.output.count(moved) == on_heap.len, result.output
	for name in on_heap {
		line := source.all_before('mut ${name} := ').count('\n') + 1
		assert result.output.contains('main.v:${line}:6: warning: ${moved}'), '${name}\n${result.output}'
	}
	c_code := os.read_file(out_path) or { panic(err) }
	assert c_code.contains('main__Entry on_stack = '), 'on_stack was moved to the heap'
	assert c_code.contains('fill(&on_stack)')
	assert c_code.contains('main__Entry forwarded_on_stack = '), 'forwarded_on_stack was moved to the heap'
	assert c_code.contains('fill(&forwarded_on_stack)')
	for name in ['option_on_stack', 'forwarded_option_on_stack', 'option_pair_on_stack'] {
		assert c_code.contains('main__Entry ${name} = '), '${name} was moved to the heap'
		assert c_code.contains('(&${name})'), '${name} is not passed by its address'
	}
	for name in on_heap {
		assert c_code.contains('main__Entry* ${name} = '), '${name} was left on the stack'
		assert c_code.contains('(${name})'), '${name} is not passed as its storage'
		assert !c_code.contains('(&${name})'), '${name} is passed as the address of its storage'
	}
}
