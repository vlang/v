module fastc

import os
import v.cmdexec

fn test_fastc_function_alias_with_nested_callback_types() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_nested_callback_alias_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), r'module main

type Notify = fn (int, fn ())
type Apply = fn (fn (int) int, int) int
type Nested = fn (fn (fn (int) int, int) int) int

fn notify(value int, callback fn ()) {
	assert value == 42
	callback()
}

fn done() {
	println("callback invoked")
}

fn increment(value int) int {
	return value + 1
}

fn apply(callback fn (int) int, value int) int {
	return callback(value)
}

fn nested(callback fn (fn (int) int, int) int) int {
	return callback(increment, 41)
}

fn main() {
	callback := Notify(notify)
	callback(42, done)
	transform := Apply(apply)
	assert transform(increment, 41) == 42
	nest := Nested(nested)
	assert nest(apply) == 42
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'callback invoked', run.output
}
