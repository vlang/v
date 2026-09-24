module v3tests

import os

fn run_assert_failure_source(name string, src string) os.Result {
	tmp := os.join_path(os.vtmp_dir(), 'v3_assert_failure_values')
	os.mkdir_all(tmp) or { panic(err) }
	source := os.join_path(tmp, name)
	os.write_file(source, src) or { panic(err) }
	return os.execute('${os.quoted_path(@VEXE)} -no-memory-limit run ${os.quoted_path(source)}')
}

// test_failed_assert_reports_operand_values checks the report of a failed assert
// outside of test files, see https://github.com/vlang/v/issues/28901 .
fn test_failed_assert_reports_operand_values() {
	result := run_assert_failure_source('main.v', "fn foo() string {
	return 'zzz'
}

fn main() {
	assert foo() == 'www'
	println('unreachable')
}
")
	assert result.exit_code == 1, result.output
	lines := result.output.trim_space().split_into_lines()
	assert lines.len == 4, result.output
	assert lines[0].ends_with("main.v:6: FAIL: fn main.main: assert foo() == 'www'"), result.output
	assert lines[1] == '   left value: foo() = zzz', result.output
	assert lines[2] == "  right value: 'www' = www", result.output
	assert lines[3] == 'V panic: Assertion failed...', result.output
}

// test_failed_asserts_in_test_files_report_operands_once checks that the values of
// failed asserts in test files are reported without evaluating the operands again.
fn test_failed_asserts_in_test_files_report_operands_once() {
	result := run_assert_failure_source('values_test.v', "struct Point {
	x int
	y int
}

fn next(mut calls []string) string {
	calls << 'next'
	return 'call \${calls.len}'
}

fn check_answer(x int) {
	assert x == 42
}

fn test_call_operand() {
	mut calls := []string{}
	assert next(mut calls) == 'call 2', 'calls: \${calls}'
}

fn test_struct_operands() {
	assert Point{1, 2} == Point{1, 3}
}

fn test_string_order() {
	assert 'abc' > 'xyz'
}

fn test_array_operands() {
	values := [3]
	assert values == [1, 2]
}

fn test_message_without_values() {
	ok := false
	assert ok, 'ok is false'
}

fn test_assert_in_helper() {
	check_answer(1)
}
")
	assert result.exit_code != 0, result.output
	output := result.output
	assert output.contains("     Left value (len: 6): `call 1`\n    Right value (len: 6): `call 2`\n        Message: calls: ['next']\n"), output
	assert output.contains('     Left value (len: 26): `Point{\n    x: 1\n    y: 2\n}`\n    Right value (len: 26): `Point{\n    x: 1\n    y: 3\n}`\n'), output
	assert output.contains('     Left value (len: 3): `abc`\n    Right value (len: 3): `xyz`\n'), output
	assert output.contains('     Left value (len: 3): `[3]`\n    Right value (len: 6): `[1, 2]`\n'), output
	assert output.contains("    assert ok, 'ok is false'\n        Message: ok is false\n"), output
	assert output.contains(': fn check_answer\n   > assert x == 42\n     Left value (len: 1): `1`\n'), output
	assert !output.contains('V panic'), output
}

// test_failed_assert_reads_left_operand_before_right checks that capturing a right
// operand with side effects does not run it before the left operand is read.
fn test_failed_assert_reads_left_operand_before_right() {
	result := run_assert_failure_source('order_test.v', "struct Box {
mut:
	n int
	s string
}

fn bump(mut b Box) int {
	b.n++
	return b.n
}

fn grow(mut b Box) string {
	b.s += 'b'
	return b.s
}

fn test_int_operands() {
	mut b := Box{}
	assert b.n == bump(mut b)
}

fn test_string_operands() {
	mut b := Box{
		s: 'a'
	}
	assert b.s == grow(mut b)
}
")
	assert result.exit_code != 0, result.output
	output := result.output
	assert output.contains('     Left value (len: 1): `0`\n    Right value (len: 1): `1`\n'), output
	assert output.contains('     Left value (len: 1): `a`\n    Right value (len: 2): `ab`\n'), output
}
