module fastc

import os
import v.cmdexec

fn test_fastc_interpolated_constant_keeps_imported_dependencies() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_constant_dependency_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'producer')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'consumer')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_constant_dependency' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'producer', 'value.v'), "module producer\npub const base = initial()\nfn initial() string { return 'ready' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'consumer', 'value.v'), r'module consumer
import producer
pub const text = "${producer.base}:suffix"
pub const copied = producer.base') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main
import consumer
fn main() {
	println(consumer.copied)
	println(consumer.text)
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ready\nready:suffix', run.output
}
