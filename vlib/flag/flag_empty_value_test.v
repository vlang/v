import flag

fn finalize_error(mut fp flag.FlagParser) string {
	rest := fp.finalize() or { return err.msg() }
	return 'no error, rest: ${rest}'
}

fn test_string_flag_accepts_an_explicitly_empty_value() {
	mut fp := flag.new_flag_parser(['flg', '--s='])
	fp.skip_executable()
	s := fp.string('s', `s`, 'default', '')
	assert s == ''
	assert fp.finalize()! == []
}

fn test_string_flag_accepts_an_empty_next_argument() {
	mut fp := flag.new_flag_parser(['--long', '', '-s', '', 'pos'])
	assert fp.string('long', 0, 'default', '') == ''
	assert fp.string('short', `s`, 'default', '') == ''
	assert fp.finalize()! == ['pos']
}

fn test_string_opt_and_string_val_return_an_explicitly_empty_value() {
	mut fp := flag.new_flag_parser(['--a=', '--b='])
	a := fp.string_opt('a', 0, '') or { 'not given' }
	assert a == ''
	b := fp.string_val('b', 0, ?string(none), '')
	if value := b {
		assert value == ''
	} else {
		assert false, '--b= should give an empty string, not none'
	}
	assert fp.finalize()! == []
}

fn test_string_multi_keeps_empty_values() {
	mut fp := flag.new_flag_parser(['--s=', '--s=x', '-s', ''])
	assert fp.string_multi('s', `s`, '') == ['', 'x', '']
	assert fp.finalize()! == []
}

fn test_empty_value_does_not_match_a_flag_with_a_longer_name() {
	mut fp := flag.new_flag_parser(['--ss=', '--s=abc'])
	assert fp.string('s', 0, 'default', '') == 'abc'
	assert fp.string('ss', 0, 'default', '') == ''
	assert fp.finalize()! == []
}

fn test_empty_value_for_a_numeric_flag_is_an_invalid_value() {
	mut fp := flag.new_flag_parser(['--n='])
	assert fp.int('n', 0, 5, '') == 5
	assert finalize_error(mut fp) == 'Invalid value `` for flag `--n`: expected an integer'
	mut fp2 := flag.new_flag_parser(['--f='])
	assert fp2.float('f', 0, 1.5, '') == 1.5
	assert finalize_error(mut fp2) == 'Invalid value `` for flag `--f`: expected a number'
}

fn test_empty_value_for_a_bool_flag_is_an_invalid_value() {
	mut fp := flag.new_flag_parser(['--b='])
	assert fp.bool('b', `b`, false, '') == false
	assert finalize_error(mut fp) == 'Invalid value `` for flag `--b`: expected `true` or `false`'
}
