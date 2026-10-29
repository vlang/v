module fastc

import os
import v.cmdexec

fn test_fastc_closures_capture_main_module_structs_and_pointers() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_main_type_capture_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

struct Counter {
mut:
	value int
}

fn make(counter &Counter) fn (int) int {
	return fn [counter] (increment int) int {
		mut target := counter
		target.value += increment
		return target.value
	}
}

fn (mut counter Counter) callback() fn () int {
	return fn [mut counter] () int {
		increment := fn [mut counter] () { counter.value++ }
		increment()
		return counter.value
	}
}

fn main() {
	mut counter := &Counter{value: 40}
	first := make(counter)
	assert first(2) == 42
	second := make(counter)
	assert second(3) == 45
	assert counter.value == 45
	bump := counter.callback()
	assert bump() == 46
	assert bump() == 47
	assert counter.value == 47
	value := Counter{value: 7}
	snapshot := fn [value] () int { return value.value }
	assert snapshot() == 7
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
