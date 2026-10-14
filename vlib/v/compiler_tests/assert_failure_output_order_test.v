module v3tests

import os

// The report of a failed assert or test is written by generated C, not by `eprintln`.
// It has to keep the order that `eprintln` keeps: what was printed before the report
// comes before it, and what is printed after the report comes after it, see
// https://github.com/vlang/v/issues/29954 .
//
// The children below write stdout and stderr into one pipe. Each of them runs with the
// buffering that the C runtime gives it, and then with stderr, stdout or both of them
// fully buffered. A fully buffered stderr is what the C runtime on Windows has for a pipe;
// setting it explicitly makes a report, that is not flushed, arrive late on every platform.
const output_order_buffer_modes = ['', 'stderr', 'stdout', 'both']

const output_order_buffer_env = 'V_OUTPUT_ORDER_BUFFER'

const output_order_buffer_setup = "import os

fn buffer_fully(stream &C.FILE) {
	// The buffer has to outlive `main`: the stream is flushed again at exit.
	unsafe { C.setvbuf(stream, &char(C.malloc(4096)), C._IOFBF, 4096) }
}

fn setup_buffers() {
	mode := os.getenv('${output_order_buffer_env}')
	if mode in ['stderr', 'both'] {
		buffer_fully(C.stderr)
	}
	if mode in ['stdout', 'both'] {
		buffer_fully(C.stdout)
	}
}
"

const output_order_program = output_order_buffer_setup + "
@[assert_continues]
fn check(n int) {
	assert n == 0, 'n is \${n}'
}

fn main() {
	setup_buffers()
	println('one')
	check(1)
	println('two')
	check(2)
	println('three')
	last := 3
	assert last == 4
	println('unreachable')
}
"

const output_order_program_lines = [
	'one',
	"FAIL: fn main.check: assert n == 0, 'n is \${n}'",
	'   left value: n = 1',
	'  right value: 0',
	'      message: n is 1',
	'two',
	"FAIL: fn main.check: assert n == 0, 'n is \${n}'",
	'   left value: n = 2',
	'  right value: 0',
	'      message: n is 2',
	'three',
	'FAIL: fn main.main: assert last == 4',
	'   left value: last = 3',
	'  right value: 4',
	'V panic: Assertion failed...',
]

const output_order_test_file = output_order_buffer_setup + "
fn testsuite_begin() {
	setup_buffers()
}

fn fails() ! {
	return error('boom')
}

fn test_a_failed_assert() {
	println('a')
	n := 1
	assert n == 2, 'n is \${n}'
}

fn test_b_failed_propagation() {
	println('b')
	fails()!
}

fn test_c_returned_error() ! {
	println('c')
	return error('returned')
}

fn test_d_passes() {
	println('d')
}
"

const output_order_test_file_lines = [
	'a',
	'fn test_a_failed_assert',
	"   > assert n == 2, 'n is \${n}'",
	'     Left value (len: 1): `1`',
	'    Right value (len: 1): `2`',
	'        Message: n is 1',
	'',
	'b',
	'fn test_b_failed_propagation failed propagation with error: boom',
	'c',
	'fn test_c_returned_error failed propagation with error: returned',
	'd',
]

struct OutputOrderChild {
	dir    string
	source string
}

fn (child OutputOrderChild) cleanup() {
	os.rmdir_all(child.dir) or {}
}

fn new_output_order_child(name string, src string) OutputOrderChild {
	dir := os.join_path(os.vtmp_dir(), 'v3_assert_output_order_${os.getpid()}_${name.all_before('.')}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	source := os.join_path(dir, name)
	os.write_file(source, src) or { panic(err) }
	return OutputOrderChild{
		dir:    dir
		source: source
	}
}

// output_order_lines returns the lines of the output of a child, without the
// `file:line: ` in front of a report: the spelling of the path depends on the platform.
fn output_order_lines(output string, source string) []string {
	marker := '${os.file_name(source)}:'
	mut lines := []string{}
	for line in output.replace('\r\n', '\n').trim_right('\n').split('\n') {
		if line.contains(marker) {
			lines << line.all_after(marker).all_after(': ')
		} else {
			lines << line
		}
	}
	return lines
}

// check_output_order builds the child once, and checks its output for every buffering.
fn check_output_order(child OutputOrderChild, expected []string) {
	base := os.join_path(child.dir, 'child')
	exe := if os.user_os() == 'windows' { '${base}.exe' } else { base }
	build := os.exec([@VEXE, '-no-memory-limit', '-o', base, child.source])
	assert build.exit_code == 0, build.output
	defer {
		os.unsetenv(output_order_buffer_env)
	}
	for mode in output_order_buffer_modes {
		os.setenv(output_order_buffer_env, mode, true)
		result := os.exec([exe])
		assert result.exit_code == 1, 'buffered: `${mode}`\n${result.output}'
		lines := output_order_lines(result.output, child.source)
		assert lines == expected, 'buffered: `${mode}`\n${result.output}'
	}
}

// output_order_windows_c returns the C, that is generated for the child on Windows.
fn output_order_windows_c(child OutputOrderChild) string {
	c_path := os.join_path(child.dir, 'child.c')
	generate := os.exec([@VEXE, '-no-memory-limit', '-os', 'windows', '-o', c_path, child.source])
	assert generate.exit_code == 0, generate.output
	c := os.read_file(c_path) or { panic(err) }
	return c.replace('\r\n', '\n')
}

// test_failed_assert_report_keeps_the_order_of_stdout_and_stderr checks a failed assert
// outside of test files, in a function that continues after it and in one that exits.
fn test_failed_assert_report_keeps_the_order_of_stdout_and_stderr() {
	child := new_output_order_child('main.v', output_order_program)
	defer {
		child.cleanup()
	}
	check_output_order(child, output_order_program_lines)
}

// test_failed_test_report_keeps_the_order_of_stdout_and_stderr checks the reports of the
// test runner: a failed assert, a failed propagation and an error that a test returns.
fn test_failed_test_report_keeps_the_order_of_stdout_and_stderr() {
	child := new_output_order_child('order_test.v', output_order_test_file)
	defer {
		child.cleanup()
	}
	check_output_order(child, output_order_test_file_lines)
}

// test_failure_reports_are_written_by_the_flushing_helpers checks the C for Windows, where
// stderr is buffered, and which the other platforms can not run: every line of a report
// goes through `v3_eprintf`, that flushes stdout before the line, and stderr after it.
fn test_failure_reports_are_written_by_the_flushing_helpers() {
	program := new_output_order_child('main.v', output_order_program)
	test_file := new_output_order_child('order_test.v', output_order_test_file)
	defer {
		program.cleanup()
		test_file.cleanup()
	}
	c := output_order_windows_c(program)
	assert c.contains('static FILE* v3_eprint_begin(void) {\n\tfflush(stdout);\n\tfflush(stderr);\n\treturn stderr;\n}\n')
	assert c.contains('static void v3_eprint_end(void) {\n\tfflush(stderr);\n}\n')
	assert c.contains('#define v3_eprintf(...) do { fprintf(v3_eprint_begin(), __VA_ARGS__); v3_eprint_end(); } while (0)\n')
	assert c.contains('static void v3_eprint_lit(const char* s) {\n\tv3_eprintf("%s", s);\n}\n')
	assert c.contains('v3_eprintf("%s: %s = %.*s\\n", "   left value", "n", ')
	assert c.contains('v3_eprintf("      message: %.*s\\n", ')
	assert !c.contains('fprintf(stderr, "%s: %s = ')
	assert !c.contains('fprintf(stderr, "      message: ')
	test_c := output_order_windows_c(test_file)
	assert test_c.contains('v3_eprintf("     Left value (len: %lld): `%.*s`\\n", ')
	assert test_c.contains('v3_eprintf("    Right value (len: %lld): `%.*s`\\n", ')
	assert test_c.contains('v3_eprintf("        Message: %.*s\\n", ')
	assert test_c.count('v3_eprintf("%s:%d: fn %s failed propagation with error: %.*s\\n", ') == 2
	assert !test_c.contains('fprintf(stderr, "     Left value')
	assert !test_c.contains('fprintf(stderr, "    Right value')
	assert !test_c.contains('fprintf(stderr, "        Message: ')
	assert !test_c.contains('fprintf(stderr, "%s:%d: fn %s failed propagation')
}
