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
