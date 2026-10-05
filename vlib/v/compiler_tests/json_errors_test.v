// Tests for `-json-errors`: the diagnostics of the compiler as one JSON object
// per line, for the tools that read them instead of a person.
import os
import json2
import encoding.utf8

struct CallSite {
	file string
	line int
	col  int
}

struct Diagnostic {
	file        string
	line        int
	col         int
	end_line    int
	end_col     int
	severity    string
	label       string
	message     string
	details     string
	called_from []CallSite
}

const failing_program = "module main

fn main() {
	a := 1
	b := a + 'x'
	println(b)
	mut unused := 3
}
"

fn testsuite_begin() {
	// The expected paths are relative to the working directory.
	os.unsetenv('VERROR_PATHS')
}

// write_project writes `files` into a new directory and returns its path.
fn write_project(name string, files map[string]string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_json_errors_${name}_${os.getpid()}')
	os.rmdir_all(dir) or {}
	for path, source in files {
		full_path := os.join_path(dir, path)
		os.mkdir_all(os.dir(full_path)) or { panic(err) }
		os.write_file(full_path, source) or { panic(err) }
	}
	// The compiler compares resolved paths with its working directory.
	return os.real_path(dir)
}

// run_in runs the compiler in `dir`, so that the paths it reports are relative to it.
fn run_in(dir string, args []string) os.Result {
	old_dir := os.getwd()
	os.chdir(dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	mut cmd := [@VEXE]
	cmd << args
	return os.exec(cmd)
}

// decode_diagnostics requires every line of `output` to be one diagnostic.
fn decode_diagnostics(output string) []Diagnostic {
	mut diagnostics := []Diagnostic{}
	for line in output.split_into_lines() {
		assert line.starts_with('{') && line.ends_with('}'), output
		// json2 accepts invalid UTF-8, which a JSON text must not have.
		assert utf8.validate_str(line), output
		diagnostics << json2.decode[Diagnostic](line) or { panic('${err}: ${line}') }
	}
	return diagnostics
}

fn test_json_errors_reports_errors_and_warnings_with_their_spans() {
	dir := write_project('spans', {
		'main.v': failing_program
	})
	defer {
		os.rmdir_all(dir) or {}
	}
	res := run_in(dir, ['-check', '-json-errors', 'main.v'])
	assert res.exit_code == 1, res.output
	assert decode_diagnostics(res.output) == [
		Diagnostic{
			file:     'main.v'
			line:     7
			col:      6
			end_line: 7
			end_col:  12
			severity: 'warning'
			message:  'unused variable: `unused`'
		},
		Diagnostic{
			file:     'main.v'
			line:     5
			col:      7
			end_line: 5
			end_col:  14
			severity: 'error'
			message:  'operator `+` cannot concatenate `int` and `string`'
		},
	]
}

// The option is accepted without `-new-compiler` and without `-check`, and a failed
// build is not retried with the compatibility compiler, whose diagnostics are text.
fn test_json_errors_of_a_build_are_only_json() {
	dir := write_project('build', {
		'main.v': failing_program
	})
	defer {
		os.rmdir_all(dir) or {}
	}
	res := run_in(dir, ['-json-errors', '-o', 'main_bin', 'main.v'])
	assert res.exit_code == 1, res.output
	diagnostics := decode_diagnostics(res.output)
	assert diagnostics.map(it.severity) == ['warning', 'error'], res.output
	assert !os.exists(os.join_path(dir, 'main_bin'))
	assert !os.exists(os.join_path(dir, 'main_bin.exe'))
}

// An error found while generic functions are specialized, after the checker, is a
// diagnostic without a position.
fn test_json_errors_reports_errors_of_the_monomorphization() {
	dir := write_project('monomorph', {
		'main.v': 'fn add[T](a T, b T) T {
	return a.plus(b, 1)
}

struct N {
	v int
}

fn (n N) plus(o N) N {
	return N{n.v + o.v}
}

fn main() {
	println(add(N{1}, N{2}))
}
'
	})
	defer {
		os.rmdir_all(dir) or {}
	}
	res := run_in(dir, ['-json-errors', '-o', 'main_bin', 'main.v'])
	assert res.exit_code == 1, res.output
	diagnostics := decode_diagnostics(res.output)
	assert diagnostics.len == 1, res.output
	assert diagnostics[0].file == ''
	assert diagnostics[0].severity == 'error'
	assert diagnostics[0].message == 'argument count mismatch for `a.plus`: expected 1, got 2'
}

fn test_json_errors_keeps_the_exit_code_of_a_program_with_warnings_only() {
	dir := write_project('warnings', {
		'main.v': 'module main\n\nfn main() {\n\tunused := 1\n\tprintln(2)\n}\n'
	})
	defer {
		os.rmdir_all(dir) or {}
	}
	res := run_in(dir, ['-check', '-json-errors', 'main.v'])
	assert res.exit_code == 0, res.output
	diagnostics := decode_diagnostics(res.output)
	assert diagnostics.len == 1, res.output
	assert diagnostics[0].severity == 'warning'
	assert diagnostics[0].message == 'unused variable: `unused`'
	// `-W` turns the warning into an error, as it does in the text form.
	strict := run_in(dir, ['-check', '-W', '-json-errors', 'main.v'])
	assert strict.exit_code == 1, strict.output
	assert decode_diagnostics(strict.output).map(it.severity) == ['error'], strict.output
}

fn test_json_errors_prints_nothing_for_a_clean_program() {
	dir := write_project('clean', {
		'main.v': "module main\n\nfn main() {\n\tprintln('ok')\n}\n"
	})
	defer {
		os.rmdir_all(dir) or {}
	}
	res := run_in(dir, ['-check', '-json-errors', 'main.v'])
	assert res.exit_code == 0, res.output
	assert res.output == ''
}

fn test_json_errors_reports_syntax_errors() {
	dir := write_project('syntax', {
		'main.v': 'module main\n\nfn main() {\n\tx := [1, 2\n\tprintln(x)\n}\n'
	})
	defer {
		os.rmdir_all(dir) or {}
	}
	res := run_in(dir, ['-check', '-json-errors', 'main.v'])
	assert res.exit_code == 1, res.output
	diagnostics := decode_diagnostics(res.output)
	assert diagnostics.len == 1, res.output
	assert diagnostics[0].file == 'main.v'
	assert diagnostics[0].line == 6
	assert diagnostics[0].col == 1
	assert diagnostics[0].severity == 'error'
	assert diagnostics[0].message == 'unexpected token `}`, expecting `]`'
}

// Quotes, a backslash and the line break of a message stay inside one JSON string,
// and the details of the text form are kept, without colors.
fn test_json_errors_escapes_messages_and_keeps_details() {
	dir := write_project('details', {
		'main.v': 'module main

struct Abc {
	a int
}

struct Abc {
	b int
}

fn main() {
	s := Abc{}
	println(s.zzz)
	println("quote \\" here" + 1)
}
'
	})
	defer {
		os.rmdir_all(dir) or {}
	}
	res := run_in(dir, ['-color', '-check', '-json-errors', 'main.v'])
	assert res.exit_code == 1, res.output
	assert !res.output.contains('\x1b'), res.output
	diagnostics := decode_diagnostics(res.output)
	duplicate := diagnostics.filter(it.message.starts_with('cannot register struct `Abc`'))
	assert duplicate.len == 1, res.output
	assert duplicate[0].line == 7
	assert duplicate[0].details.starts_with('main.v:3:1: details: another declaration was found here\n'), res.output
	assert diagnostics.any(it.message == 'type `Abc` has no field named `zzz`.\n1 possibility: `b`.'), res.output
	// The span of the last error covers the string literal with the escaped quote.
	quoted := diagnostics.filter(it.line == 14 && it.col == 10)
	assert quoted.len == 1, res.output
	assert quoted[0].end_col == 29
}

// The scanner quotes the bytes it cannot read. Bytes that are not UTF-8 would make a
// strict JSON reader reject the line, so they are reported as U+FFFD.
fn test_json_errors_of_a_malformed_source_file_are_valid_utf8() {
	dir := write_project('malformed', {
		'lone_byte.v': 'module main\n\nfn main() {\n\t\xff\n}\n'
		'cut_off.v':   'module main\n\nfn main() {\n\tx\xe2\x82 := 2\n}\n'
	})
	defer {
		os.rmdir_all(dir) or {}
	}
	for file in ['lone_byte.v', 'cut_off.v'] {
		res := run_in(dir, ['-check', '-json-errors', file])
		assert res.exit_code == 1, res.output
		assert utf8.validate_str(res.output), res.output.bytes().hex()
		assert res.output.contains('\\ufffd'), res.output
		diagnostics := decode_diagnostics(res.output)
		invalid := diagnostics.filter(it.message.starts_with('invalid character `'))
		assert invalid.len == 1, res.output
		assert invalid[0].file == file
		assert invalid[0].line == 4
		assert invalid[0].message.contains('\ufffd'), res.output
	}
}

// The severity is `error`, `warning` or `notice`; the label of the text form is kept
// when it says more.
fn test_json_errors_severity_of_builder_errors() {
	dir := write_project('builder', {
		'missing.v':   'import nonexistent_mod_xyz\n\nfn main() {\n\tnonexistent_mod_xyz.f()\n}\n'
		'duplicate.v': 'fn f() {}\n\nfn f() {}\n\nfn main() {\n\tf()\n}\n'
	})
	defer {
		os.rmdir_all(dir) or {}
	}
	missing := run_in(dir, ['-check', '-json-errors', 'missing.v'])
	assert missing.exit_code == 1, missing.output
	import_errors := decode_diagnostics(missing.output)
	assert import_errors.len == 1, missing.output
	assert import_errors[0].file == 'missing.v'
	assert import_errors[0].line == 1
	assert import_errors[0].severity == 'error'
	assert import_errors[0].label == 'builder error'
	assert import_errors[0].message == 'cannot import module "nonexistent_mod_xyz" (not found)'
	duplicate := run_in(dir, ['-check', '-json-errors', 'duplicate.v'])
	assert duplicate.exit_code == 1, duplicate.output
	redefinitions := decode_diagnostics(duplicate.output)
	assert redefinitions.map(it.severity) == ['error', 'error', 'error'], duplicate.output
	assert redefinitions.map(it.label) == ['builder error', 'conflicting declaration',
		'conflicting declaration'], duplicate.output
	assert redefinitions[0].message == 'redefinition of function `f`'
	assert redefinitions[1..].map(it.line) == [1, 3], duplicate.output
}

// Every error is printed: the text form stops after 20 of them with a note for the
// reader, which is not a diagnostic.
fn test_json_errors_reports_every_error() {
	mut source := 'module main\n\nfn main() {\n'
	for i in 0 .. 30 {
		source += "\tx${i} := ${i} + 'a'\n\tprintln(x${i})\n"
	}
	source += '}\n'
	dir := write_project('many', {
		'main.v': source
	})
	defer {
		os.rmdir_all(dir) or {}
	}
	res := run_in(dir, ['-check', '-json-errors', 'main.v'])
	assert res.exit_code == 1, res.output
	assert decode_diagnostics(res.output).len == 30, res.output
	limited := run_in(dir, ['-check', '-json-errors', '-message-limit', '3', 'main.v'])
	assert limited.exit_code == 1, limited.output
	assert decode_diagnostics(limited.output).len == 3, limited.output
}

// The files of a project are named relative to the working directory, as in the text
// form, and an error inside a template names the `\$tmpl` call that included it.
fn test_json_errors_names_module_files_and_template_call_sites() {
	dir := write_project('project', {
		'v.mod':           "Module {\n\tname: 'project'\n}\n"
		'mymod/mymod.v':   "module mymod\n\npub fn hi() int {\n\treturn 'no'\n}\n"
		'templates/t.txt': 'hello @missing_var\n'
		'main.v':          "module main

import mymod

fn render() string {
	return \$tmpl('templates/t.txt')
}

fn main() {
	mymod.hi()
	println(render())
}
"
	})
	defer {
		os.rmdir_all(dir) or {}
	}
	res := run_in(dir, ['-check', '-json-errors', '.'])
	assert res.exit_code == 1, res.output
	diagnostics := decode_diagnostics(res.output)
	in_module := diagnostics.filter(it.file == 'mymod/mymod.v')
	assert in_module.len == 1, res.output
	assert in_module[0].line == 4
	assert in_module[0].message == 'cannot use `string` as type `int` in return argument'
	assert in_module[0].called_from.len == 0
	in_template := diagnostics.filter(it.file == 'templates/t.txt')
	assert in_template.len > 0, res.output
	assert in_template[0].message.starts_with('undefined ident: `missing_var`'), res.output
	assert in_template[0].called_from == [
		CallSite{
			file: 'main.v'
			line: 6
			col:  9
		},
	], res.output
	// VERROR_PATHS=absolute applies to the JSON form as it does to the text form.
	os.setenv('VERROR_PATHS', 'absolute', true)
	absolute := run_in(dir, ['-check', '-json-errors', '.'])
	os.unsetenv('VERROR_PATHS')
	expected := os.real_path(os.join_path(dir, 'mymod', 'mymod.v')).replace('\\', '/')
	assert decode_diagnostics(absolute.output).any(it.file == expected), absolute.output
}

// An argument of the program that `v run` starts is not a compiler option.
fn test_json_errors_after_a_run_input_belongs_to_the_program() {
	dir := write_project('run', {
		'main.v': 'module main\n\nimport os\n\nfn main() {\n\tunused := 1\n\tprintln(os.args[1..])\n}\n'
	})
	defer {
		os.rmdir_all(dir) or {}
	}
	res := run_in(dir, ['-nocolor', 'run', 'main.v', '-json-errors'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('main.v:6:2: warning: unused variable: `unused`'), res.output
	assert res.output.contains("['-json-errors']"), res.output
}
