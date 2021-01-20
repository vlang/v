module main

import os
import os.cmdline
import testing

fn main() {
	args := os.args.clone()
	if os.args.last() == 'test' {
		show_usage()
		return
	}
	args_to_executable := args[1..]
	args_before := cmdline.options_before(args_to_executable, ['test'])
	args_after := cmdline.options_after(args_to_executable, ['test'])
	if args_after.join(' ') == 'v' {
		eprintln('`v test v` has been deprecated.')
		eprintln('Use `v test-all` instead.')
		exit(1)
	}
	mut ts := testing.new_test_session(args_before.join(' '))
	for targ in args_after {
		if os.exists(targ) && targ.ends_with('_test.v') {
			ts.files << targ
			continue
		}
		if os.is_dir(targ) {
			// Fetch all tests from the directory
			ts.files << os.walk_ext(targ.trim_right(os.path_separator), '_test.v')
			continue
		}
		eprintln('\nUnrecognized test file `$targ` .\n `v test` can only be used with folders and/or _test.v files.\n')
		show_usage()
		exit(1)
	}
	testing.header('Testing...')
	ts.test()
	println(ts.benchmark.total_message('Ran all V _test.v files'))
	if ts.failed {
		exit(1)
	}
}

fn show_usage() {
	println('Usage:')
	println('   A)')
	println('      v test folder/ : run all v tests in the given folder.')
	println('      v -stats test folder/ : the same, but print more stats.')
	println('   B)')
	println('      v test file_test.v : run test functions in a given test file.')
	println('      v -stats test file_test.v : as above, but with more stats.')
	println('   NB: you can also give many and mixed folder/ file_test.v arguments after `v test` .')
	println('')
}
