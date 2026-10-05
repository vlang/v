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
	return build_program_with_flags(source, output, mode, [])
}

// build_program_with_flags is build_program with more compiler `flags`.
fn build_program_with_flags(source string, output string, mode string, flags []string) os.Result {
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
	mut args := ['-new-compiler', '-no-retry-compilation', '-v', '-nocache']
	args << flags
	args << ['-o', output, source]
	return cmdexec.run_with_timeout(vexe, args, 120_000)
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

// library_bodies_worker_threads returns how many worker threads the compiler had
// once the late bodies were checked.
fn library_bodies_worker_threads(output string) int {
	line := output.all_after('mu library bodies').all_before('\n')
	if !line.contains('worker threads:') {
		return -1
	}
	return line.all_after('worker threads:').all_before(')').trim_space().int()
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

fn test_serial_build_checks_late_bodies_without_worker_threads() {
	root := library_bodies_test_root('serial')
	saved_jobs := os.getenv_opt('VJOBS')
	defer {
		if value := saved_jobs {
			os.setenv('VJOBS', value, true)
		} else {
			os.unsetenv('VJOBS')
		}
		os.rmdir_all(root) or {}
	}
	// More than one job, whatever the machine has: a worker pool that the late
	// check started would have threads.
	os.setenv('VJOBS', '4', true)
	source := os.join_path(root, 'main.v')
	os.write_file(source, program_that_uses_little_of_os)!
	output := os.join_path(root, 'main')
	build := build_program_with_flags(source, output, 'late', ['-no-parallel'])
	assert build.exit_code == 0, build.output
	assert library_bodies_checked_late(build.output) > 0, build.output
	assert library_bodies_worker_threads(build.output) == 0, build.output
	run := cmdexec.run(output, [])
	assert run.exit_code == 0, run.output
	assert run.output == 'b.v\n', run.output
}

fn test_build_checks_the_functions_that_markused_keeps_with_the_others() {
	root := library_bodies_test_root('seeded')
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	// markused keeps the callback that the parser hands to its workers, and no name
	// in this program leads to it.
	os.write_file(source, 'import v.parser

fn main() {
	println(sizeof(parser.Parser) > 0)
}
')!
	output := os.join_path(root, 'main')
	build := build_program(source, output, '')
	assert build.exit_code == 0, build.output
	assert library_bodies_left_unchecked(build.output) > 100, build.output
	// A body that is checked late costs another run of markused.
	assert library_bodies_checked_late(build.output) == 0, build.output
	run := cmdexec.run(output, [])
	assert run.exit_code == 0, run.output
	assert run.output == 'true\n', run.output
}

fn test_build_leaves_unreachable_runtime_bodies_unchecked() {
	root := library_bodies_test_root('runtime')
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	// Without an import, the library of this program is `builtin`, `strings` and
	// `strconv`, of which it calls little.
	os.write_file(source, 'fn main() {
	a := []u8{len: 10}
	println(a.len)
}
')!
	output := os.join_path(root, 'main')
	build := build_program(source, output, '')
	assert build.exit_code == 0, build.output
	assert library_bodies_left_unchecked(build.output) > 300, build.output
	assert library_bodies_checked_late(build.output) == 0, build.output
	run := cmdexec.run(output, [])
	assert run.exit_code == 0, run.output
	assert run.output == '10\n', run.output
}

fn test_build_checks_the_runtime_functions_of_lowered_constructs_at_once() {
	root := library_bodies_test_root('constructs')
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	// No name in this program leads to the runtime functions that its maps,
	// slices, interpolations, errors and loops are lowered to. markused keeps them,
	// and the check takes them from it: none is left for a later check.
	os.write_file(source, "struct Point {
	x int
	y int
}

fn half(n int) !int {
	if n % 2 != 0 {
		return error('odd: \${n}')
	}
	return n / 2
}

fn first(names []string) ?string {
	if names.len == 0 {
		return none
	}
	return names[0]
}

fn main() {
	mut ages := map[string]int{}
	ages['ada'] = 36
	ages['alan'] = 41
	mut names := ages.keys()
	names.sort()
	println(names[..1])
	for name, age in ages {
		if name == 'ada' {
			println('\${name:-6}|\${age:4}|\${f64(age) / 3:.2f}')
		}
	}
	println(half(8) or { -1 })
	println(half(7) or { -1 })
	println(first(names) or { 'nobody' })
	println(first([]string{}) or { 'nobody' })
	text := '  padded  '
	println('[' + text.trim_space() + ']')
	println('alan' in ages)
	println(Point{1, 2})
	mut squares := []int{cap: 4}
	for i in 0 .. 4 {
		squares << i * i
	}
	println(squares[1..3])
	println(text.trim_space()[1..3].to_upper())
}
")!
	output := os.join_path(root, 'main')
	build := build_program(source, output, '')
	assert build.exit_code == 0, build.output
	assert library_bodies_left_unchecked(build.output) > 200, build.output
	assert library_bodies_checked_late(build.output) == 0, build.output
	run := cmdexec.run(output, [])
	assert run.exit_code == 0, run.output
	assert run.output == "['ada']
ada   |  36|12.00
4
-1
ada
nobody
[padded]
true
Point{
    x: 1
    y: 2
}
[1, 4]
AD
", run.output
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
