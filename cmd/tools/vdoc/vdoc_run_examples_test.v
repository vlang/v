import os

fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
	os.chdir(@VEXEROOT)!
}

fn run_example_docs(flag string, example string) os.Result {
	path := os.join_path('cmd', 'tools', 'vdoc', 'testdata', 'run_examples_${example}', 'main.v')
	return os.exec([@VEXE, 'doc', '-comments', flag, path])
}

fn test_check_examples_does_not_execute_failing_assertion() {
	res := run_example_docs('-check-examples', 'bad')
	assert res.exit_code == 0, res.output
	assert res.output.contains('module main'), res.output
	assert res.output.contains('Example: assert 5 * 5 == 77'), res.output
	assert res.output.contains('Found 1 ok examples.'), res.output
}

fn test_run_examples_good() {
	res := run_example_docs('-unsafe-run-examples', 'good')
	assert res.exit_code == 0, res.output
	assert res.output.contains('module main'), res.output
	assert res.output.contains('fn abc()'), res.output
	assert res.output.contains("abc just prints 'xyz'"), res.output
	assert res.output.contains('and should succeed'), res.output
	assert res.output.contains('Example: assert 5 * 5 == 25'), res.output
	assert res.output.contains('Found 1 ok examples.'), res.output
}

fn test_run_examples_bad() {
	res := run_example_docs('-unsafe-run-examples', 'bad')
	assert res.exit_code != 0, res.output
	assert res.output.contains('error in documentation example'), res.output
	// V1 includes the evaluated value here; V3 currently reports the expression only.
	assert res.output.contains(' left value: 5 * 5'), res.output
	assert res.output.contains('right value: 77'), res.output
	assert res.output.contains('V panic: Assertion failed...'), res.output
	assert res.output.contains('module main'), res.output
	assert res.output.contains('Example: assert 5 * 5 == 77'), res.output
	assert res.output.contains('Found 1 failing examples.'), res.output
}
