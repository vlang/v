import os

// Expect has to be installed for the test.
const expect_exe = os.find_abs_path_of_executable('expect') or {
	eprintln('skipping test, since expect is missing')
	exit(0)
}
// Directory that contains the Expect scripts used in the test.
const expect_tests_path = os.join_path(@VMODROOT, 'examples', 'password', 'tests')

fn test_password_input() {
	correct := os.execute(os.join_path(expect_tests_path, 'correct.expect'))
	assert correct.exit_code == 0, correct.output

	incorrect := os.execute(os.join_path(expect_tests_path, 'incorrect.expect'))
	assert incorrect.exit_code == 0, incorrect.output

	expected_out := 'Enter your password : '
	mut res := os.execute('${os.join_path(expect_tests_path, 'output_from_expect_arg.expect')} "${expected_out}"')
	assert res.exit_code == 0, res.output

	not_exptectd_out := 'Enter your passwords : '
	res = os.execute('${os.join_path(expect_tests_path, 'output_from_expect_arg.expect')} "${not_exptectd_out}"')
	assert res.exit_code == 1, res.output
}
