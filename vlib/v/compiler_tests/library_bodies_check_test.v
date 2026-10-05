import os
import v.cmdexec

const vexe = os.join_path(@VMODROOT, 'v' + $if windows { '.exe' } $else { '' })

fn library_bodies_test_root(name string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_library_bodies_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

// build_program builds `source` without a module cache, which checks whole modules,
// and with `mode` in V_CHECK_LIBRARY_BODIES.
fn build_program(source string, output string, mode string) os.Result {
	saved := os.getenv_opt('V_CHECK_LIBRARY_BODIES')
	defer {
		if value := saved {
			os.setenv('V_CHECK_LIBRARY_BODIES', value, true)
		} else {
			os.unsetenv('V_CHECK_LIBRARY_BODIES')
		}
	}
	if mode == '' {
		os.unsetenv('V_CHECK_LIBRARY_BODIES')
	} else {
		os.setenv('V_CHECK_LIBRARY_BODIES', mode, true)
	}
	return cmdexec.run_with_timeout(vexe, ['-new-compiler', '-no-retry-compilation', '-v', '-nocache',
		'-o', output, source], 120_000)
}

fn library_bodies_left_unchecked(output string) int {
	line := output.all_after('mu library bodies').all_before('\n')
	if !line.contains('left unchecked') {
		return -1
	}
	return line.all_before('left unchecked').trim_space().int()
}

fn library_bodies_checked_late(output string) int {
	line := output.all_after('mu library bodies').all_before('\n')
	if !line.contains('checked late') {
		return -1
	}
	return line.all_after('left unchecked,').all_before('checked late').trim_space().int()
}

const program_that_uses_little_of_os = "import os

fn main() {
	println(os.base('/a/b.v'))
}
"

fn test_build_leaves_unreachable_library_bodies_unchecked() {
	root := library_bodies_test_root('unreachable')
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, program_that_uses_little_of_os)!
	output := os.join_path(root, 'main')
	build := build_program(source, output, '')
	assert build.exit_code == 0, build.output
	// Most of `os` is not called by a program that takes the base name of a path.
	assert library_bodies_left_unchecked(build.output) > 100, build.output
	assert library_bodies_checked_late(build.output) == 0, build.output
	run := cmdexec.run(output, [])
	assert run.exit_code == 0, run.output
	assert run.output == 'b.v\n', run.output
}

fn test_build_checks_every_library_body_on_request() {
	root := library_bodies_test_root('all')
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, program_that_uses_little_of_os)!
	output := os.join_path(root, 'main')
	build := build_program(source, output, 'all')
	assert build.exit_code == 0, build.output
	assert library_bodies_left_unchecked(build.output) == -1, build.output
	run := cmdexec.run(output, [])
	assert run.output == 'b.v\n', run.output
}

fn test_build_checks_library_bodies_that_markused_reaches_later() {
	root := library_bodies_test_root('late')
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, program_that_uses_little_of_os)!
	output := os.join_path(root, 'main')
	// With `late`, no name leads to a library body: every one that the program
	// reaches is found by markused, checked then, and compiled.
	build := build_program(source, output, 'late')
	assert build.exit_code == 0, build.output
	assert library_bodies_checked_late(build.output) > 0, build.output
	assert library_bodies_left_unchecked(build.output) > 100, build.output
	run := cmdexec.run(output, [])
	assert run.exit_code == 0, run.output
	assert run.output == 'b.v\n', run.output
}

fn test_build_checks_unused_functions_of_the_program() {
	root := library_bodies_test_root('program')
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, "import os

fn never_called() int {
	return 'not an int'
}

fn main() {
	println(os.base('/a/b.v'))
}
")!
	build := build_program(source, os.join_path(root, 'main'), '')
	assert build.exit_code != 0, build.output
	assert build.output.contains('cannot use `string` as type `int` in return argument'), build.output
	assert build.output.contains('main.v:4:'), build.output
}

fn test_build_checks_unused_functions_of_project_modules() {
	root := library_bodies_test_root('project')
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module {\n\tname: 'project'\n\tversion: '0.0.1'\n}\n")!
	os.mkdir_all(os.join_path(root, 'helper'))!
	os.write_file(os.join_path(root, 'helper', 'helper.v'), "module helper

pub fn used() int {
	return 1
}

pub fn never_called() int {
	return 'not an int'
}
")!
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main

import helper

fn main() {
	println(helper.used())
}
')!
	build := build_program(source, os.join_path(root, 'main'), '')
	assert build.exit_code != 0, build.output
	assert build.output.contains('cannot use `string` as type `int` in return argument'), build.output
	assert build.output.contains('helper.v:8:'), build.output
}
