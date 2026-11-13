module fastc

import os
import v.cmdexec

// `d.callbacks = []fn () {}` assigns an empty array of a function type; the `fn`
// after `[]` is the element type, not a function literal.
fn test_empty_function_array_assignment_to_field() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_fn_array_field_assignment_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fn_array_field_assignment' }")!
	os.write_file(os.join_path(root, 'main.v'), "module main
struct Document {
mut:
	cleanup []fn ()
}
fn main() {
	mut d := Document{}
	d.cleanup << fn () {
		println('cleanup')
	}
	pending := d.cleanup
	d.cleanup = []fn (){}
	for callback in pending {
		callback()
	}
	mut local := []fn (){}
	local = []fn (){}
	println('\${d.cleanup.len} \${local.len}')
}
")!
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_' + backend)
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-o', program, root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
		assert run.output.trim_space() == 'cleanup\n0 0', '${backend}: ${run.output}'
	}
}
