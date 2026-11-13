module fastc

import os
import v.cmdexec

fn test_fastc_comptime_zero_preserves_types_and_usable_defaults() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_comptime_zero_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

type Count = i64
enum State {
	ready = 7
	done
}
struct Row { count int = 3 }

fn zero[T]() T { return $zero(T) }
fn take(value i64) i64 { return value + 1 }

fn main() {
	assert zero[int]() == 0
	assert zero[string]() == ""
	assert !zero[bool]()
	assert zero[Count]() == Count(0)
	assert zero[State]() == State.ready
	assert zero[Row]().count == 3
	assert isnil(zero[&Row]())
	mut values := zero[[]int]()
	values << 42
	assert values[0] == 42
	mut entries := zero[map[string]int]()
	entries["answer"] = 42
	assert entries["answer"] == 42
	assert take($zero(i64)) == 1
	assert $zero(i64) + 2 == 2
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
