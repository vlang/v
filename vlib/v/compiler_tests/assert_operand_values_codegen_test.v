module v3tests

import os

// run_failing_assert compiles a program with a failing assertion and runs it.
fn run_failing_assert(name string, code string) !os.Result {
	tmp := os.join_path(os.vtmp_dir(), name)
	os.mkdir_all(tmp)!
	source := os.join_path(tmp, 'main.v')
	os.write_file(source, code)!
	return os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache run ${os.quoted_path(source)}')
}

// test_failing_assert_on_strings_reports_the_values covers #28901: a failed
// comparison on strings printed its source line, but not the two values.
fn test_failing_assert_on_strings_reports_the_values() {
	run := run_failing_assert('v3_assert_string_values', "fn foo() string {\n\treturn 'zzz'\n}\n\nfn main() {\n\tassert foo() == 'www'\n}\n")!
	assert run.exit_code != 0, run.output
	assert run.output.contains('V panic: Assertion failed...'), run.output
	assert run.output.contains('   left value: foo() = zzz'), run.output
	assert run.output.contains("  right value: 'www' = www"), run.output
}

// test_failing_assert_evaluates_string_operands_once keeps printing a string
// operand from running it a second time.
fn test_failing_assert_evaluates_string_operands_once() {
	run := run_failing_assert('v3_assert_string_operand_once', "fn main() {\n\tmut values := ['a']\n\tassert values.pop() == 'b'\n}\n")!
	assert run.exit_code != 0, run.output
	assert run.output.contains('   left value: values.pop() = a'), run.output
}

// test_failing_assert_on_bools_reports_the_values prints a bool operand on the
// value side, and a bool literal as the label it is written as.
fn test_failing_assert_on_bools_reports_the_values() {
	run := run_failing_assert('v3_assert_bool_values', 'fn main() {\n\tflag := true\n\tassert flag == false\n}\n')!
	assert run.exit_code != 0, run.output
	assert run.output.contains('   left value: flag = true'), run.output
	assert run.output.contains('  right value: false'), run.output
}

// test_failing_assert_keeps_numeric_values guards the operand values that
// already worked before strings and bools were added.
fn test_failing_assert_keeps_numeric_values() {
	run := run_failing_assert('v3_assert_numeric_values', 'fn main() {\n\tassert 5 * 5 == 77\n}\n')!
	assert run.exit_code != 0, run.output
	assert run.output.contains('   left value: 5 * 5'), run.output
	assert run.output.contains('  right value: 77'), run.output
}
