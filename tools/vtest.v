module main

import (
	os
	testing
)

pub fn main() {
	args := os.args
	if args.last() == 'test' {
		println('Usage:')
		println('   A)')
		println('      v test folder/ : run all v tests in the given folder.')
		println('      v -stats test folder/ : the same, but print more stats.')
		println('   B)')
		println('      v test file_test.v : run test functions in a given test file.')
		println('      v -stats test file_test.v : as above, but with more stats.')
		println('   NB: you can also give many and mixed folder/ file_test.v arguments after test.')
		println('')
		return
	}

	args_string := args[1..].join(' ')
	args_before := args_string.all_before('test ')
	args_after  := args_string.all_after('test ')

	if args_after == 'v' {
		eprintln('`v test v` has been deprecated.')
		eprintln('Use `v test-compiler` instead.')
		exit(1)
	}

	mut ts := testing.new_test_session(args_before)
	for targ in args_after.split(' ') {
		if os.exists(targ) && targ.ends_with('_test.v') {
			ts.files << targ
			continue
		}
		if os.is_dir(targ) {
			// Fetch all tests from the directory
			ts.files << os.walk_ext( targ.trim_right(os.path_separator), '_test.v')
			continue
		}
		println('Unrecognized test file $targ .')
	}

	println('Testing...')
	ts.test()
	println('----------------------------------------------------------------------------')
	println( ts.benchmark.total_message('running V _test.v files') )
	if ts.failed {
		exit(1)
	}
}

