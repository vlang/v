module main

import os

// Programs whose unused private declarations a check has to report as a build
// does. Some are only told apart by markused: an exported function, or one
// marked `@[markused]`, is used although no V code names it; a function passed
// as a value is used when the code passing it is, and a function called only
// by an unused one is not reported.
const build_parity_programs = {
	'mixed':         "module main

const limit = 3

const used_limit = 4

fn helper() int {
	return used_limit
}

fn unused_caller() int {
	return only_from_unused()
}

fn only_from_unused() int {
	return 2
}

@[export: 'exported_helper']
fn exported_helper() int {
	return 5
}

@[markused]
fn kept_helper() int {
	return 6
}

pub fn public_helper() int {
	return 7
}

fn main() {
	println(helper())
}
"
	'kept_only':     "module main

@[export: 'exported_helper']
fn exported_helper() int {
	return 5
}

@[markused]
fn kept_helper() int {
	return 6
}

fn main() {
	println('hi')
}
"
	'callbacks':     "module main

type Visit = fn (name string) string

fn shout(name string) string {
	return name.to_upper()
}

fn visit_all(names []string, visit Visit) []string {
	return names.map(visit(it))
}

fn main() {
	println(visit_all(['a'], shout))
}
"
	'dead_callback': "module main

type Visit = fn (name string) string

fn shout(name string) string {
	return name.to_upper()
}

fn register(visit Visit) {
	println(visit('a'))
}

fn unused_setup() {
	register(shout)
}

fn main() {
	register(fn (name string) string {
		return name
	})
}
"
}

// write_program writes `source` as the main.v of a new directory and returns its path.
fn write_program(name string, source string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_check_warnings_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	path := os.join_path(dir, 'main.v')
	os.write_file(path, source) or { panic(err) }
	return path
}

fn check_output(name string, source string) os.Result {
	path := write_program(name, source)
	defer {
		os.rmdir_all(os.dir(path)) or {}
	}
	return os.execute('${os.quoted_path(@VEXE)} -new-compiler -check -nocolor ${os.quoted_path(path)}')
}

// check_and_build_notices returns the notices of a check and of a build of the same program.
fn check_and_build_notices(name string, source string) ([]string, []string) {
	path := write_program(name, source)
	defer {
		os.rmdir_all(os.dir(path)) or {}
	}
	check := os.execute('${os.quoted_path(@VEXE)} -new-compiler -check -nocolor ${os.quoted_path(path)}')
	assert check.exit_code == 0, check.output
	exe := path.all_before_last('.v')
	build := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -gc none -nocolor -o ${os.quoted_path(exe)} ${os.quoted_path(path)}')
	assert build.exit_code == 0, build.output
	return notice_lines(check.output), notice_lines(build.output)
}

// check_library_output checks `source` as the only file of a module that no
// program imports, the way an editor checks it: on its own, as a library.
fn check_library_output(name string, source string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_check_warnings_${name}_${os.getpid()}', name)
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(os.dir(dir)) or {}
	}
	os.write_file(os.join_path(dir, '${name}.v'), source) or { panic(err) }
	return os.execute('${os.quoted_path(@VEXE)} -new-compiler -shared -check -nocolor ${os.quoted_path(dir)}')
}

fn notice_lines(output string) []string {
	mut lines := output.split_into_lines().filter(it.contains(': notice: '))
	lines.sort()
	return lines
}

// A check prints the warnings of a program without errors too: they are
// diagnostics like the others, and an editor shows nothing it is not given.
fn test_check_prints_the_warnings_of_a_program_without_errors() {
	res := check_output('clean', 'module main\n\nfn main() {\n\tunused := 1\n\tprintln(2)\n}\n')
	assert res.exit_code == 0, res.output
	assert res.output.contains('warning: unused variable: `unused`'), res.output
}

fn test_check_prints_warnings_next_to_errors() {
	res := check_output('failing', "module main\n\nfn main() {\n\tunused := 1\n\tbad := 'a' + 1\n\tprintln(bad)\n}\n")
	assert res.exit_code == 1, res.output
	assert res.output.contains('warning: unused variable: `unused`'), res.output
	assert res.output.contains('error: infix expr'), res.output
}

// A build reports the private functions and constants that nothing uses; a
// check has to report them too.
fn test_check_reports_unused_private_declarations() {
	res := check_output('unused', "module main\n\nconst limit = 3\n\nfn helper() int {\n\treturn 1\n}\n\nfn main() {\n\tprintln('hi')\n}\n")
	assert res.exit_code == 0, res.output
	assert res.output.contains('notice: unused constant: `limit`'), res.output
	assert res.output.contains('notice: unused function: `helper`'), res.output
}

fn test_check_reports_the_unused_declarations_a_build_reports() {
	for name, source in build_parity_programs {
		check, build := check_and_build_notices(name, source)
		assert check == build, '${name}:\ncheck: ${check}\nbuild: ${build}'
	}
}

// An editor shows the diagnostics of a program while its errors are being
// fixed, the unused declarations among them. With errors, only what nothing
// names is reported: a call the checker could not resolve still names its
// function, which may be used after all.
fn test_check_reports_unused_private_declarations_next_to_errors() {
	res := check_output('unused_failing', 'module main\n\nconst limit = 3\n\nfn helper() int {\n\treturn 1\n}\n\nfn called_badly(x int) int {\n\treturn x\n}\n\nfn passed_to_unknown() {}\n\nfn main() {\n\ty := called_badly(1, 2)\n\tprintln(y)\n\tprintln(missing(passed_to_unknown))\n}\n')
	assert res.exit_code == 1, res.output
	assert res.output.contains('error: expected 1 argument, but got 2'), res.output
	assert res.output.contains('notice: unused constant: `limit`'), res.output
	assert res.output.contains('notice: unused function: `helper`'), res.output
	assert !res.output.contains('`called_badly`'), res.output
	assert !res.output.contains('unused function: `passed_to_unknown`'), res.output
}

const library_with_unused = "module lib

pub const shared_limit = 1

const limit = 3

const used_limit = 4

pub fn api() int {
	return used_limit + helper()
}

fn helper() int {
	return 1
}

fn orphan() int {
	return 2
}

@[export: 'lib_exported']
fn exported() int {
	return 5
}

@[markused]
fn kept() int {
	return 6
}

pub fn unused_api() {}
"

// A module that no program imports is checked on its own, as a library. Its
// public declarations are what other programs use, and what a private one serves
// cannot be told without a program: the private ones that nothing names are
// reported, and one exported to C or kept with `@[markused]` is used.
fn test_check_reports_the_unused_private_declarations_of_a_library() {
	res := check_library_output('lib', library_with_unused)
	assert res.exit_code == 0, res.output
	assert notice_lines(res.output).len == 2, res.output
	assert res.output.contains('notice: unused constant: `limit`'), res.output
	assert res.output.contains('notice: unused function: `orphan`'), res.output
}

// With errors too, as an editor shows them while they are being fixed.
fn test_check_reports_the_unused_private_declarations_of_a_library_with_errors() {
	res := check_library_output('lib', library_with_unused + "\nfn broken() int {\n\treturn 'a'\n}\n\npub fn calls_broken() int {\n\treturn broken()\n}\n")
	assert res.exit_code == 1, res.output
	assert res.output.contains('error:'), res.output
	assert res.output.contains('notice: unused constant: `limit`'), res.output
	assert res.output.contains('notice: unused function: `orphan`'), res.output
	assert !res.output.contains('`shared_limit`'), res.output
	assert !res.output.contains('`exported`'), res.output
}
