import os

const child_source = r'
module main

import os

struct Item {
	value int
}

fn recurse(depth int) int {
	mut buf := [64]int{}
	buf[depth % 64] = depth
	return recurse(depth + 1) + buf[(depth + 1) % 64]
}

fn main() {
	mode := if os.args.len > 1 { os.args[1] } else { "main" }
	match mode {
		"thread" {
			t := spawn recurse(0)
			println(t.wait())
		}
		"nil" {
			p := unsafe { &Item(nil) }
			println(p.value)
		}
		else {
			println(recurse(0))
		}
	}
}
'

fn run_child(binary string, mode string) os.Result {
	// The messages go to stderr, so merge it into the captured output.
	return os.exec(['/bin/sh', '-c', '${os.quoted_path(binary)} ${mode} 2>&1'])
}

fn test_stack_overflow_prints_a_message() {
	$if windows {
		return
	}
	work_dir := os.join_path(os.vtmp_dir(), 'stack_overflow_test_${os.getpid()}')
	os.mkdir_all(work_dir)!
	defer {
		os.rmdir_all(work_dir) or {}
	}
	source := os.join_path(work_dir, 'child.v')
	binary := os.join_path(work_dir, 'child')
	os.write_file(source, child_source)!
	compile := os.exec([@VEXE, '-o', binary, source])
	assert compile.exit_code == 0, compile.output
	for mode in ['main', 'thread'] {
		res := run_child(binary, mode)
		assert res.exit_code != 0, '${mode}: ${res.output}'
		assert res.output.contains('V panic: stack overflow'), '${mode}: ${res.output}'
	}
	// Other faults keep their message: V's segmentation fault message, or the one of
	// the TCC `-bt` runtime, that handled them before.
	res := run_child(binary, 'nil')
	assert res.exit_code != 0, res.output
	assert res.output.contains('segmentation fault')
		|| res.output.contains('invalid memory access'), res.output
	assert !res.output.contains('stack overflow'), res.output
}
