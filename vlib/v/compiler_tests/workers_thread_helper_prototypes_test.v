import os

const workers_prototypes_vexe = @VEXE

const workers_prototypes_program = "import v.workers

fn main() {
	mut pool := workers.new(1)
	pool.close()
	println('ok')
}
"

// The functions that vlib/v/workers calls in pthread_helper.o have no header in
// a program that links that object: its C has to declare them. A C compiler that
// rejects a call to an undeclared function, as gcc and clang do, does not compile
// the program otherwise, and the compiler is such a program. On Linux the file
// that declares them includes a system header too, for `prctl`.
fn test_a_program_that_imports_v_workers_declares_the_functions_of_its_thread_helper() {
	root := os.join_path(os.vtmp_dir(), 'v3_workers_prototypes_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, workers_prototypes_program)!
	c_file := os.join_path(root, 'main.c')
	// C generation also exercises the Linux header/declaration interaction on
	// other hosts; compiling and executing that target requires a Linux host.
	generated := os.exec([workers_prototypes_vexe, '-nocache', '-os', 'linux', '-o', c_file, main_file])
	assert generated.exit_code == 0, generated.output
	lines := os.read_lines(c_file)!
	for name in ['v3_pthread_zero', 'v3_pthread_create', 'v3_pthread_is_current'] {
		mentions := lines.filter(it.contains('${name}('))
		assert mentions.len > 1, name
		// The first line that names it is its declaration, not a call.
		first := mentions[0].trim_space()
		assert first.ends_with(');') && !first.contains('=') && !first.starts_with('return '), '${name}: ${first}'
	}
	$if !linux {
		return
	}
	// With a C compiler that takes an undeclared function for an error, when there is one.
	if os.find_abs_path_of_executable('gcc') or { '' } == '' {
		return
	}
	ran := os.exec([workers_prototypes_vexe, '-nocache', '-cc', 'gcc', '-cflags',
		'-Werror=implicit-function-declaration', 'run', main_file])
	assert ran.exit_code == 0, ran.output
	assert ran.output.trim_space() == 'ok', ran.output
}
