import flag

fn test_stop_on_first_non_flag_issue_30699_repro() {
	mut fp := flag.new_flag_parser(['flg', 'pos1', '--b'])
	fp.skip_executable()
	fp.stop_on_first_non_flag()
	b := fp.bool('b', `b`, false, '', flag.FlagConfig{})
	rest := fp.finalize()!
	assert !b
	assert rest == ['pos1', '--b']
}

fn test_default_behavior_preserved_without_stop_on_first_non_flag() {
	mut fp := flag.new_flag_parser(['flg', 'pos1', '--b'])
	fp.skip_executable()
	b := fp.bool('b', `b`, false, '', flag.FlagConfig{})
	rest := fp.finalize()!
	assert b
	assert rest == ['pos1']
}

fn test_flags_before_positional_parsed_and_after_ignored() {
	mut fp := flag.new_flag_parser(['flg', '--a', '-n', '42', 'pos1', '--b', '-x', 'ignored'])
	fp.skip_executable()
	fp.stop_on_first_non_flag()
	a := fp.bool('a', `a`, false, '')
	n := fp.int('n', `n`, 0, '')
	b := fp.bool('b', `b`, false, '')
	x := fp.string('x', `x`, '', '')
	rest := fp.finalize()!
	assert a
	assert n == 42
	assert !b
	assert x == ''
	assert rest == ['pos1', '--b', '-x', 'ignored']
}

fn test_single_dash_treated_as_non_flag() {
	mut fp := flag.new_flag_parser(['flg', '-', '--b'])
	fp.skip_executable()
	fp.stop_on_first_non_flag()
	b := fp.bool('b', `b`, false, '')
	rest := fp.finalize()!
	assert !b
	assert rest == ['-', '--b']
}

fn test_unknown_flag_before_first_non_flag_errors() {
	mut fp := flag.new_flag_parser(['flg', '--unknown', 'pos1', '--b'])
	fp.skip_executable()
	fp.stop_on_first_non_flag()
	b := fp.bool('b', `b`, false, '')
	if _ := fp.finalize() {
		assert false
	} else {
		assert err.msg().contains('Unknown flag `--unknown`')
	}
	assert !b
}

fn test_string_flag_with_equal_sign() {
	mut fp := flag.new_flag_parser(['flg', '--s=hello', 'pos1', '--b'])
	fp.skip_executable()
	fp.stop_on_first_non_flag()
	s := fp.string('s', `s`, '', '')
	b := fp.bool('b', `b`, false, '')
	rest := fp.finalize()!
	assert s == 'hello'
	assert !b
	assert rest == ['pos1', '--b']
}

fn test_multiple_positionals_and_multiple_flags_after() {
	mut fp := flag.new_flag_parser(['flg', 'pos1', 'pos2', '--b', '-s', 'test'])
	fp.skip_executable()
	fp.stop_on_first_non_flag()
	b := fp.bool('b', `b`, false, '')
	s := fp.string('s', `s`, '', '')
	rest := fp.finalize()!
	assert !b
	assert s == ''
	assert rest == ['pos1', 'pos2', '--b', '-s', 'test']
}

fn test_dashdash_terminator_with_stop_on_first_non_flag() {
	mut fp := flag.new_flag_parser(['flg', '--a', '--', 'pos1', '--b'])
	fp.skip_executable()
	fp.stop_on_first_non_flag()
	a := fp.bool('a', `a`, false, '')
	b := fp.bool('b', `b`, false, '')
	rest := fp.finalize()!
	assert a
	assert !b
	assert rest == ['pos1', '--b']
}
