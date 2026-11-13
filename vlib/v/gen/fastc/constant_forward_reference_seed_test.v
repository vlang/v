module fastc

import os
import v.cmdexec

// A struct default that reads a constant makes the constants phase seed their types in
// order first. A file that reads another file's constant and also reads one of its own
// constants before declaring it (`copied` above `later`) must keep the declared type of
// that constant, as the in-order parse of the file gives it.
fn test_seeded_constant_file_keeps_types_of_its_forward_references() ! {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_constant_forward_seed_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'constant_forward_seed' }")!
	os.write_file(os.join_path(root, 'external.v'), 'module main

const external = 2
')!
	os.write_file(os.join_path(root, 'forward.v'), 'module main

const uses_other = external + 1
const copied = later
const later = make_f64()

fn make_f64() f64 {
	return 3.5
}
')!
	os.write_file(os.join_path(root, 'main.v'), "module main

struct Config {
	scale int = external
}

fn main() {
	config := Config{}
	println('\${copied} \${later} \${uses_other} \${config.scale}')
}
")!
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_' + backend)
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-o', program, root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
		assert run.output.trim_space() == '3.5 3.5 3 2', '${backend}: ${run.output}'
	}
}
