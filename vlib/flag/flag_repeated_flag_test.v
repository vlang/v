import flag

fn finalize_error(mut fp flag.FlagParser) string {
	rest := fp.finalize() or { return err.msg() }
	return 'no error, rest: ${rest}'
}

fn test_repeated_bool_flag_is_accepted() {
	mut fp := flag.new_flag_parser(['flg', '--b', '--b'])
	fp.skip_executable()
	assert fp.bool('b', `b`, false, '') == true
	assert fp.finalize()! == []
}

fn test_repeated_bool_flag_last_value_wins() {
	mut fp := flag.new_flag_parser(['-a', '--a=false', '--b=false', '-b', '--c', 'true', '--c',
		'false', '-d', '-d'])
	assert fp.bool('a', `a`, false, '') == false
	assert fp.bool('b', `b`, false, '') == true
	assert fp.bool('c', 0, true, '') == false
	assert fp.bool_opt('d', `d`, '')! == true
	assert fp.finalize()! == []
}

fn test_repeated_short_bool_flags_in_clusters_are_all_consumed() {
	mut fp := flag.new_flag_parser(['-vv', '-av', '-v', 'pos'])
	assert fp.bool('verbose', `v`, false, '') == true
	assert fp.bool('all', `a`, false, '') == true
	assert fp.finalize()! == ['pos']
}

fn test_repeated_bool_flag_keeps_positionals() {
	mut fp := flag.new_flag_parser(['--b', 'pos1', '--b', 'pos2'])
	assert fp.bool('b', `b`, false, '') == true
	assert fp.finalize()! == ['pos1', 'pos2']
}

fn test_repeated_int_flag_last_value_wins() {
	mut fp := flag.new_flag_parser(['flg', '--n', '1', '--n', '2'])
	fp.skip_executable()
	assert fp.int('n', `n`, 0, '') == 2
	assert fp.finalize()! == []
	mut fp2 := flag.new_flag_parser(['a', '-n', '1', 'b', '--n=3', 'c'])
	assert fp2.int_opt('n', `n`, '')! == 3
	assert fp2.finalize()! == ['a', 'b', 'c']
}

fn test_repeated_float_and_string_flags_last_value_wins() {
	mut fp := flag.new_flag_parser(['--f', '1.5', '--f=2.5', '-s', 'a', '--s=b', '--t', 'x', '--t',
		'y'])
	assert fp.float('f', 0, 0.0, '') == 2.5
	assert fp.string('s', `s`, '', '') == 'b'
	assert fp.string_opt('t', 0, '')! == 'y'
	assert fp.finalize()! == []
}

fn test_every_value_of_a_repeated_numeric_flag_is_checked() {
	mut fp := flag.new_flag_parser(['--n', 'abc', '--n', '2'])
	assert fp.int('n', 0, 7, '') == 7
	assert finalize_error(mut fp) == 'Invalid value `abc` for flag `--n`: expected an integer'
	mut fp2 := flag.new_flag_parser(['--b=yes', '--b'])
	assert fp2.bool('b', 0, false, '') == false
	assert finalize_error(mut fp2) == 'Invalid value `yes` for flag `--b`: expected `true` or `false`'
}

fn test_multi_flags_still_collect_every_value() {
	mut fp := flag.new_flag_parser(['-n', '1', '--n=2', '-s', 'a', '--s', 'b', '-f', '0.5', '-f',
		'1.5'])
	assert fp.int_multi('n', `n`, '') == [1, 2]
	assert fp.string_multi('s', `s`, '') == ['a', 'b']
	assert fp.float_multi('f', `f`, '') == [0.5, 1.5]
	assert fp.finalize()! == []
}
