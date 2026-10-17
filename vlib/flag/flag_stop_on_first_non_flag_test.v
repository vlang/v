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
	fp.stop_on_first_non_flag('-n', '-x')
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

fn test_value_flags_do_not_depend_on_getter_order() {
	for bool_first in [true, false] {
		mut fp := flag.new_flag_parser(['app', '--number', '42', '--enabled', 'item', '--enabled'])
		fp.skip_executable()
		fp.stop_on_first_non_flag('--number')
		mut enabled := false
		mut number := 0
		if bool_first {
			enabled = fp.bool('enabled', 0, false, '')
			number = fp.int('number', 0, 0, '')
		} else {
			number = fp.int('number', 0, 0, '')
			enabled = fp.bool('enabled', 0, false, '')
		}
		assert enabled
		assert number == 42
		assert fp.finalize()! == ['item', '--enabled']
	}
}

fn test_boolean_before_positional_is_not_a_value_option() {
	mut fp := flag.new_flag_parser(['app', '--enabled', 'item', '--number', '42'])
	fp.skip_executable()
	fp.stop_on_first_non_flag('--number')
	assert fp.int('number', 0, 0, '') == 0
	assert fp.bool('enabled', 0, false, '')
	assert fp.finalize()! == ['item', '--number', '42']
}

fn test_value_options_preserve_empty_and_dash_values() {
	for value in ['', '-', '-42', 'false'] {
		mut fp := flag.new_flag_parser(['app', '-s', value, '--enabled', 'item'])
		fp.skip_executable()
		fp.stop_on_first_non_flag('--string', '-s')
		assert fp.bool('enabled', 0, false, '')
		assert fp.string('string', `s`, '', '') == value
		assert fp.finalize()! == ['item']
	}
}

fn test_dashdash_after_positional_is_preserved() {
	mut fp := flag.new_flag_parser(['app', 'item', '--', '--enabled'])
	fp.skip_executable()
	fp.stop_on_first_non_flag()
	assert !fp.bool('enabled', 0, false, '')
	assert fp.finalize()! == ['item', '--', '--enabled']
}

fn test_explicit_boolean_value_before_positional() {
	mut fp := flag.new_flag_parser(['app', '--enabled', 'false', '--number', '42', 'item'])
	fp.skip_executable()
	fp.stop_on_first_non_flag('--number')
	assert fp.int('number', 0, 0, '') == 42
	assert !fp.bool('enabled', 0, true, '')
	assert fp.finalize()! == ['item']
}

fn test_boolean_cluster_leaves_following_word_positional() {
	mut fp := flag.new_flag_parser(['app', '-ab', 'false', '--enabled'])
	fp.skip_executable()
	fp.stop_on_first_non_flag()
	assert !fp.bool('enabled', 0, false, '')
	assert fp.bool('a', `a`, false, '')
	assert fp.bool('b', `b`, false, '')
	assert fp.finalize()! == ['false', '--enabled']
}
