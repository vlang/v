module fastc

import os
import v.cmdexec

// `f(mut value)` for a `mut x Interface` parameter hands the callee the caller's value, so
// what the callee changes through the interface is seen by the caller.
fn test_fastc_mut_argument_to_a_mut_interface_parameter_is_the_value_itself() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_mut_interface_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_mut_interface' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

interface Sink {
mut:
	send(x int)
}

struct Fake {
mut:
	n int
}

fn (mut f Fake) send(x int) {
	f.n += x
}

struct Holder {
mut:
	fake Fake
}

struct Link {
mut:
	passes int
}

fn (mut l Link) service(mut t Sink, x int) {
	t.send(x)
	l.passes++
}

fn drive(mut t Sink) {
	t.send(5)
}

fn main() {
	mut l := Link{}
	mut b := Fake{}
	l.service(mut b, 3)
	drive(mut b)
	mut h := Holder{}
	drive(mut h.fake)
	println(b.n.str() + " " + h.fake.n.str() + " " + l.passes.str())
}
') or {
		panic(err)
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '8 5 1'
}
