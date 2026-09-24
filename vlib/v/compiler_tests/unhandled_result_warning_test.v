// Tests for the warning about a call whose Result nothing handles. Its error is
// lost: the program runs on, or prints the Result as if it were the value. `!`
// at the end passes the error on and an `or {}` block handles it, as does
// `if x := call() {`; returning the call from a function that returns a Result,
// or spawning it, passes it on too. Where the checker already reports the call
// with an error (an assignment, an operand, a typed argument...), no warning is
// added on top.
import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v.v')
const unhandled_v3_bin = os.join_path(os.temp_dir(), 'v3_unhandled_result_test_${os.getpid()}')

const prelude = "module main

import os

struct Box {}

fn (b Box) risky() !int {
	return error('box')
}

fn Box.make() !Box {
	return error('make')
}

struct Point {
	x int
}

fn fails() !int {
	return error('nope')
}

fn fails_void() ! {
	return error('void')
}

fn maybe() ?int {
	return none
}

fn gen[T]() !T {
	return error('gen')
}

fn takes(n int) {}

fn use(values ...int) {}
"

const either_hint = 'so it should have either an `or {}` block, or `!` at the end'
const defer_hint = 'so it should have an `or {}` block at the end'

enum Want {
	warning       // a warning at the call
	defer_warning // the same, where only an `or {}` block can handle it
	error_only    // the error the checker already reports, and no warning
	nothing       // handled or passed on: nothing at all
}

struct Case {
	name  string
	code  string // the body of main; «call» marks the call checked
	extra string // top-level code, when the call is there instead
	want  Want
}

const cases = [
	// the call as a statement: its error is dropped
	Case{'stmt', '«fails()»', '', .warning},
	Case{'stmt_void', '«fails_void()»', '', .warning},
	Case{'stmt_method', 'b := Box{}\n\t«b.risky()»', '', .warning},
	Case{'stmt_static', '«Box.make()»', '', .warning},
	// a function of a module is reported at its name, as the checker does elsewhere
	Case{'stmt_module', "os.«ls('.')»", '', .warning},
	Case{'stmt_generic', '«gen[int]()»', '', .warning},
	Case{'stmt_closure', 'f := fn () !int {\n\t\treturn 1\n\t}\n\t«f()»', '', .warning},
	Case{'stmt_in_if', 'if os.args.len > 0 {\n\t\t«fails()»\n\t}', '', .warning},
	Case{'stmt_in_for', 'for _ in 0 .. 1 {\n\t\t«fails()»\n\t}', '', .warning},
	Case{'stmt_in_match', 'match os.args.len {\n\t\t1 { «fails()» }\n\t\telse {}\n\t}', '', .warning},
	Case{'stmt_in_unsafe', 'unsafe {\n\t\t«fails()»\n\t}', '', .warning},
	Case{'stmt_in_result_fn', 'g() or {}', 'fn g() ! {\n\t«fails()»\n}\n', .warning},
	Case{'stmt_paren', '(«fails()»)', '', .warning},
	Case{'stmt_in_defer', 'defer {\n\t\t«fails()»\n\t}', '', .defer_warning},
	// a value that nothing unwraps: the Result itself is printed or kept
	Case{'arg_println', 'println(«fails()»)', '', .warning},
	Case{'arg_dump', 'dump(«fails()»)', '', .warning},
	Case{'interpolation', "println('v=\${«fails()»}')", '', .warning},
	Case{'assign_blank', '_ = «fails()»', '', .warning},
	Case{'array_literal', 'a := [«fails()»]\n\tprintln(a)', '', .warning},
	// already an error
	Case{'arg_int', 'takes(«fails()»)', '', .error_only},
	Case{'arg_variadic', 'use(1, «fails()»)', '', .error_only},
	Case{'receiver', 'println(«fails()».str())', '', .error_only},
	Case{'assign_decl', 'x := «fails()»\n\tprintln(x)', '', .error_only},
	Case{'assign_mut', 'mut n := 0\n\tn = «fails()»\n\tprintln(n)', '', .error_only},
	Case{'assign_plus', 'mut n := 0\n\tn += «fails()»\n\tprintln(n)', '', .error_only},
	Case{'infix', 'println(«fails()» + 1)', '', .error_only},
	Case{'array_append', 'mut a := [1]\n\ta << «fails()»\n\tprintln(a)', '', .error_only},
	Case{'map_literal', "m := {\n\t\t'a': «fails()»\n\t}\n\tprintln(m)", '', .error_only},
	Case{'struct_field', 'p := Point{\n\t\tx: «fails()»\n\t}\n\tprintln(p)', '', .error_only},
	Case{'index', 'a := [1, 2]\n\tprintln(a[«fails()»])', '', .error_only},
	Case{'return_int_fn', 'println(g())', 'fn g() int {\n\treturn «fails()»\n}\n', .error_only},
	// handled, or passed on
	Case{'return_result_fn', 'println(g() or { 0 })', 'fn g() !int {\n\treturn «fails()»\n}\n', .nothing},
	Case{'spawn', 't := spawn «fails()»\n\tprintln(t.wait() or { 0 })', '', .nothing},
	Case{'or_block', 'x := «fails()» or { 0 }\n\tprintln(x)', '', .nothing},
	Case{'or_stmt', '«fails()» or { println(err) }', '', .nothing},
	Case{'or_panic', '«fails_void()» or { panic(err) }', '', .nothing},
	Case{'bang_in_result_fn', 'g() or {}', 'fn g() ! {\n\t«fails()»!\n}\n', .nothing},
	Case{'bang_arg', 'g() or {}', 'fn g() ! {\n\tprintln(«fails()»!)\n}\n', .nothing},
	Case{'bang_interpolation', 'g() or {}', "fn g() ! {\n\tprintln('v=\${«fails()»!}')\n}\n", .nothing},
	Case{'if_guard', 'if x := «fails()» {\n\t\tprintln(x)\n\t}', '', .nothing},
	Case{'or_arg', 'println(«fails()» or { 0 })', '', .nothing},
	Case{'or_in_defer', 'defer {\n\t\t«fails()» or {}\n\t}', '', .nothing},
	Case{'or_interpolation', "println('v=\${«fails()» or { 0 }}')", '', .nothing},
	// an Option, not a Result: nothing is lost by dropping it
	Case{'option_stmt', '«maybe()»', '', .nothing},
	Case{'option_arg', 'println(«maybe()»)', '', .nothing},
]

fn build_v3() string {
	if !os.is_executable(unhandled_v3_bin) {
		res := os.execute('${os.quoted_path(vexe)} -gc none -path ${os.quoted_path('${vlib_dir}|@vlib|@vmodules')} -o ${os.quoted_path(unhandled_v3_bin)} ${os.quoted_path(v3_src)}')
		assert res.exit_code == 0, res.output
	}
	return unhandled_v3_bin
}

fn testsuite_end() {
	os.rm(unhandled_v3_bin) or {}
}

fn unmarked(text string) string {
	return text.replace('«', '').replace('»', '')
}

fn program(c Case) string {
	return prelude + '\n' + unmarked(c.extra) + '\nfn main() {\n\t' + unmarked(c.code) + '\n}\n'
}

// call_position returns the line and column, 1-based, of the marked call.
fn call_position(c Case) (int, int) {
	text := if c.extra.contains('«') {
		prelude + '\n' + c.extra.all_before('«')
	} else {
		prelude + '\n' + unmarked(c.extra) + '\nfn main() {\n\t' + c.code.all_before('«')
	}
	lines := text.split('\n')
	return lines.len, lines.last().runes().len + 1
}

// write_program writes `source` as main.v of a new directory, and returns it.
fn write_program(name string, source string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_unhandled_result_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), source) or { panic(err) }
	return dir
}

// build_output builds the program in `dir`, as `v run` does before running it:
// a check alone prints no warning when there is no error.
fn build_output(v3 string, dir string) string {
	return os.execute('${os.quoted_path(v3)} -nocache -gc none -nocolor -o ${os.quoted_path(os.join_path(dir, 'prog'))} ${os.quoted_path(os.join_path(dir, 'main.v'))}').output
}

// diagnostics_at returns the diagnostics reported on `line` of main.v.
fn diagnostics_at(output string, line int) []string {
	mut found := []string{}
	for l in output.split_into_lines() {
		parts := l.split_nth(':', 4)
		if parts.len == 4 && parts[0].ends_with('main.v') && parts[1].int() == line {
			found << '${parts[2]}:${parts[3].trim_space()}'
		}
	}
	return found
}

fn test_a_call_whose_result_nothing_handles_is_warned_about() {
	v3 := build_v3()
	mut wrong := []string{}
	for c in cases {
		dir := write_program(c.name, program(c))
		output := build_output(v3, dir)
		os.rmdir_all(dir) or {}
		line, col := call_position(c)
		at := diagnostics_at(output, line)
		warnings := at.filter(it.contains('warning:'))
		errors := at.filter(it.contains('error:'))
		ok := match c.want {
			.warning {
				warnings.len == 1 && warnings[0].starts_with('${col}:')
					&& warnings[0].contains('returns `!') && warnings[0].ends_with(either_hint)
					&& errors.len == 0
			}
			.defer_warning {
				warnings.len == 1 && warnings[0].starts_with('${col}:')
					&& warnings[0].ends_with(defer_hint) && errors.len == 0
			}
			.error_only {
				errors.len > 0 && warnings.len == 0
			}
			.nothing {
				at.len == 0
			}
		}
		if !ok {
			wrong << '${c.name} (${c.want}) at ${line}:${col}: ${at}'
		}
	}
	assert wrong.len == 0, wrong.join('\n')
}

// The exact words: those of the errors the checker gives these calls elsewhere,
// with the call named as it is written.
fn test_the_warning_names_the_call_and_its_result_type() {
	v3 := build_v3()
	dir := write_program('names', prelude + "\nfn main() {\n\tfails()\n\tb := Box{}\n\tb.risky()\n\tos.ls('.')\n\tfails_void()\n}\n")
	defer {
		os.rmdir_all(dir) or {}
	}
	output := build_output(v3, dir)
	lines := output.split_into_lines()
	for expected in ['warning: fails() returns `!int`, ${either_hint}',
		'warning: b.risky() returns `!int`, ${either_hint}',
		'warning: os.ls() returns `![]string`, ${either_hint}'] {
		assert lines.any(it.ends_with(expected)), '${expected}\n${output}'
	}
	assert lines.filter(it.contains('warning:') && it.contains('returns `!')).len == 4, output
	assert !output.contains('error:'), output
}

// `v run` shows the warning and runs the program, which goes on without the error.
fn test_v_run_warns_and_runs_the_program() {
	v3 := build_v3()
	dir := write_program('run', prelude + "\nfn main() {\n\tfails()\n\tprintln('after the call')\n}\n")
	defer {
		os.rmdir_all(dir) or {}
	}
	res := os.execute('${os.quoted_path(v3)} -nocache -gc none run ${os.quoted_path(os.join_path(dir, 'main.v'))}')
	assert res.exit_code == 0, res.output
	assert res.output.contains('warning: fails() returns `!int`, ${either_hint}'), res.output
	assert res.output.contains('after the call'), res.output
}

// With -W, warnings are errors: this one too.
fn test_the_warning_is_an_error_with_w() {
	v3 := build_v3()
	dir := write_program('werror', prelude + '\nfn main() {\n\tfails()\n}\n')
	defer {
		os.rmdir_all(dir) or {}
	}
	res := os.execute('${os.quoted_path(v3)} -nocache -W -check -nocolor ${os.quoted_path(os.join_path(dir, 'main.v'))}')
	assert res.exit_code != 0, res.output
	assert res.output.contains('error: fails() returns `!int`, ${either_hint}'), res.output
}
