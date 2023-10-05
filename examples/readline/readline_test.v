import os

fn test_password_input() {
	$if windows {
		eprintln('Input test for windows are not yet implemented.')
		return
	}
	expect_test_path := os.join_path(@VMODROOT, 'examples', 'readline', 'tests')

	correct := os.execute(os.join_path(expect_test_path, 'readline.expect'))
	if correct.exit_code != 0 {
		assert false, correct.output
	}

	// Test using input values that were passed to the expect script as arg.
	send_a := 'a'
	expect_a := 'got 97' // readline output for `a`
	a_res := os.execute('${os.join_path(expect_test_path, 'readline_from_expect_arg.expect')} ${send_a} "${expect_a}"')
	assert a_res.exit_code == 0

	send_b := 'b'
	b_res := os.execute('${os.join_path(expect_test_path, 'readline_from_expect_arg.expect')} ${send_b} "${expect_a}"')
	assert b_res.exit_code == 1
	assert b_res.output.contains('got 98')
}
