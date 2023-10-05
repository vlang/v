import os

fn test_password_input() {
	$if windows {
		eprintln('Input test for windows are not yet implemented.')
		return
	}
	expect_tests_path := os.join_path(@VMODROOT, 'examples', 'password', 'tests')

	correct := os.execute(os.join_path(expect_tests_path, 'correct.expect'))
	if correct.exit_code != 0 {
		assert false, correct.output
	}

	incorrect := os.execute(os.join_path(expect_tests_path, 'incorrect.expect'))
	if incorrect.exit_code != 0 {
		assert false, incorrect.output
	}

	expect_output := 'Enter your password : '
	mut res := os.execute('${os.join_path(expect_tests_path, 'output_from_expect_arg.expect')} "${expect_output}"')
	assert res.exit_code == 0

	wrong_expect_output := 'Enter your passwords : '
	res = os.execute('${os.join_path(expect_tests_path, 'output_from_expect_arg.expect')} "${wrong_expect_output}"')
	assert res.exit_code == 1
	assert res.output.contains('Enter your password : != Enter your passwords : ')
}
