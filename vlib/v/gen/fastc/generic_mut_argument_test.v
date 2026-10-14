module fastc

import os
import v.cmdexec

fn fastc_generic_fixture(name string, source string) string {
	root := os.join_path(os.vtmp_dir(), 'fastc_generic_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_generic' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	return root
}

const fastc_generic_sink = 'module main

struct Sink {
mut:
	n int
}

fn (mut s Sink) send(x int) {
	s.n += x
}

struct Holder {
mut:
	sink Sink
}

fn pass[S](mut st S, x int) {
	st.send(x)
}
'

// `f(mut x)` instantiates `f` for x's type, so the body runs on the caller's value.
fn test_fastc_generic_called_with_a_mut_argument_is_instantiated() {
	$if !macos && !linux {
		return
	}
	root := fastc_generic_fixture('mut', fastc_generic_sink + '
fn main() {
	mut s := Sink{}
	pass(mut s, 2)
	pass(mut s, 3)
	println(s.n)
}
')
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '5'
}

// A call no instance is made for (here the argument is a field, which the call scan does
// not resolve) reaches the stub, which must say so rather than return as if it had run.
fn test_fastc_generic_called_without_an_instance_reports_the_call() {
	$if !macos && !linux {
		return
	}
	root := fastc_generic_fixture('stub', fastc_generic_sink + '
fn main() {
	mut h := Holder{}
	pass(mut h.sink, 3)
	println(h.sink.n)
}
')
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code != 0, run.output
	assert run.output.contains('generic function `pass` was called without a concrete instance'), run.output
}

// A parameter named like a generic function is the parameter: `pass(mut s)` calls the
// callback it holds, not the generic `pass`.
fn test_fastc_parameter_shadowing_a_generic_is_called_as_the_parameter() {
	$if !macos && !linux {
		return
	}
	root := fastc_generic_fixture('shadow', fastc_generic_sink + '
fn bump(mut s Sink, x int) {
	s.n += 10 * x
}

fn invoke(pass fn (mut Sink, int), mut s Sink) {
	pass(mut s, 1)
}

fn main() {
	mut s := Sink{}
	invoke(bump, mut s)
	pass(mut s, 2)
	println(s.n)
}
')
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '12'
}

// A function literal's own `s` ends with the literal: the `pass(mut s)` after it is
// instantiated for the enclosing `s`.
fn test_fastc_generic_after_a_function_literal_uses_the_enclosing_variable() {
	$if !macos && !linux {
		return
	}
	root := fastc_generic_fixture('literal', fastc_generic_sink + '
struct Other {
mut:
	pad [4]i64
	n   int
}

fn (mut o Other) send(x int) {
	o.n += x
}

fn main() {
	mut s := Sink{}
	cb := fn () {
		mut s := Other{}
		pass(mut s, 5)
		println(s.n)
	}
	cb()
	pass(mut s, 2)
	println(s.n)
}
')
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '5\n2'
}

// A parameter shadows a generic only inside its function: a constant initialized after
// it with a call to that generic gets an instance for the call.
fn test_fastc_generic_called_after_a_function_with_a_shadowing_parameter_is_instantiated() {
	$if !macos && !linux {
		return
	}
	root := fastc_generic_fixture('after_shadow', 'module main

fn transform[T](x T) T {
	return x
}

fn hold(transform fn (int) int) int {
	return transform(2)
}

const answer = transform(1)

fn double(x int) int {
	return x * 2
}

fn main() {
	println(answer)
	println(hold(double))
}
')
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '1\n4'
}

// A generic left to on-demand instantiation is still never called through a parameter of its
// name: `transform(2)` calls the callback, with the callback's argument and result types.
fn test_fastc_parameter_shadowing_an_on_demand_generic_is_called_as_the_parameter() {
	$if !macos && !linux {
		return
	}
	root := fastc_generic_fixture('on_demand_shadow', 'module main

struct Box {
	v int
}

fn transform[T](x T) T {
	return x
}

fn hold(transform fn (int) int) int {
	a := transform(2)
	println(transform(3))
	return a + transform(4)
}

fn double(x int) int {
	return x * 2
}

fn main() {
	b := Box{
		v: 3
	}
	println(transform(b.v))
	println(hold(double))
}
')
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '3\n6\n12'
}

// A single-file build calls a callback parameter, also one named like a generic, through
// the parameter, with or without the real builtin.
fn test_fastc_single_file_calls_callback_parameters() {
	$if !macos && !linux {
		return
	}
	root := fastc_generic_fixture('single_file_callback', 'module main

fn transform[T](x T) T {
	return x
}

fn hold(transform fn (int) int) int {
	return transform(2)
}

fn apply(cb fn (int) int) int {
	return cb(3)
}

fn double(x int) int {
	return x * 2
}

fn main() {
	println(transform(1))
	println(hold(double))
	println(apply(double))
}
')
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	program := os.join_path(root, 'program')
	for defines in [[]string{}, ['-d', 'fastc_real_builtin']] {
		build := cmdexec.run(@VEXE, ['-b', 'fastc', ...defines, '-o', program, source])
		assert build.exit_code == 0, '${defines}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '1\n4\n6', defines.str()
	}
}

// A function literal's parameters, and those of a function returning a function type, shadow
// a generic of the same name: `transform(2)` calls the callback.
fn test_fastc_literal_parameter_shadowing_a_generic_is_called_as_the_parameter() {
	$if !macos && !linux {
		return
	}
	root := fastc_generic_fixture('literal_shadow', 'module main

fn transform[T](x T) T {
	return x
}

fn double(x int) int {
	return x * 2
}

fn make(transform fn (int) int) fn (int) int {
	println(transform(5))
	return double
}

fn main() {
	apply := fn (transform fn (int) int) int {
		return transform(2)
	}
	check := fn (transform fn (int) int) ?int {
		return transform(3)
	}
	run := fn (transform fn (int) int) {
		println(transform(4))
	}
	println(transform(1))
	println(apply(double))
	println(check(double) or { 0 })
	run(double)
	make(double)
}
')
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '1\n4\n6\n8\n10'
}

fn test_fastc_generic_in_top_level_constant_does_not_inherit_parameter_shadowing() {
	$if !macos && !linux {
		return
	}
	root := fastc_generic_fixture('constant_scope', 'module main
fn transform[T](value T) T { return value }
fn hold(transform fn (int) int) {}
const answer = transform(7)
fn main() { println(answer) }
')
	defer {
		os.rmdir_all(root) or {}
	}
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_${backend}')
		build := cmdexec.run(@VEXE, ['-b', backend, '-gc', 'none', '-nocache', '-o', program, root])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '7', run.output
	}
}
