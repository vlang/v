module fastc

import os
import v.cmdexec

// A function value, spelled `fn (...)` or through an alias, is already a pointer. Where a
// `voidptr` is expected (`isnil`, a fn-typed parameter) it is passed as it is, not boxed
// into fresh storage, which would make `isnil` false for a nil hook.
fn test_fastc_function_values_are_passed_as_pointers() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_fn_values_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_fn_values' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

type Hook = fn (x int)

struct Runner {
mut:
	hook Hook = unsafe { nil }
}

fn on_tick(x int) {}

fn edit(name string, change fn (string) string) string {
	return change(name)
}

fn main() {
	mut r := Runner{}
	before := isnil(r.hook)
	r.hook = on_tick
	println(before.str() + " " + isnil(r.hook).str())
	println(edit("ok", fn (s string) string {
		return s + "!"
	}))
}
') or {
		panic(err)
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true false\nok!'
}
