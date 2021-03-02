import flag

fn test_if_flag_not_given_return_default_values() {
	mut fp := flag.new_flag_parser([])
	assert false == fp.bool('a_bool', 0, false, '')
	assert 42 == fp.int('an_int', 0, 42, '')
	assert 1.0 == fp.float('a_float', 0, 1.0, '')
	assert 'stuff' == fp.string('a_string', 0, 'stuff', '')
}

fn test_could_define_application_name_and_version() {
	mut fp := flag.new_flag_parser([])
	fp.application('test app')
	fp.version('0.0.42')
	fp.description('some text')
	assert fp.application_name == 'test app'
	assert fp.application_version == '0.0.42'
	assert fp.application_description == 'some text'
}

fn test_bool_flags_do_not_need_an_value() {
	mut fp := flag.new_flag_parser(['--a_bool'])
	assert true == fp.bool('a_bool', 0, false, '')
}

fn test_flags_could_be_defined_with_eq() {
	mut fp := flag.new_flag_parser([
		'--an_int=42',
		'--a_float=2.0',
		'--bool_without',
		'--a_string=stuff',
		'--a_bool=true',
	])
	assert 42 == fp.int('an_int', 0, 0o666, '')
	assert true == fp.bool('a_bool', 0, false, '')
	assert true == fp.bool('bool_without', 0, false, '')
	assert 2.0 == fp.float('a_float', 0, 1.0, '')
	assert 'stuff' == fp.string('a_string', 0, 'not_stuff', '')
}

fn test_values_could_be_defined_without_eq() {
	mut fp := flag.new_flag_parser([
		'--an_int',
		'42',
		'--a_float',
		'2.0',
		'--bool_without',
		'--a_string',
		'stuff',
		'--a_bool',
		'true',
	])
	assert 42 == fp.int('an_int', 0, 0o666, '')
	assert true == fp.bool('a_bool', 0, false, '')
	assert true == fp.bool('bool_without', 0, false, '')
	assert 2.0 == fp.float('a_float', 0, 1.0, '')
	assert 'stuff' == fp.string('a_string', 0, 'not_stuff', '')
}

fn test_values_could_be_defined_mixed() {
	mut fp := flag.new_flag_parser([
		'--an_int',
		'42',
		'--a_float=2.0',
		'--bool_without',
		'--a_string',
		'stuff',
		'--a_bool=true',
	])
	assert 42 == fp.int('an_int', 0, 0o666, '')
	assert true == fp.bool('a_bool', 0, false, '')
	assert true == fp.bool('bool_without', 0, false, '')
	assert 2.0 == fp.float('a_float', 0, 1.0, '')
	assert 'stuff' == fp.string('a_string', 0, 'not_stuff', '')
}

fn test_beaware_for_argument_names_with_same_prefix() {
	mut fp := flag.new_flag_parser([
		'--short',
		'5',
		'--shorter=7',
	])
	assert 5 == fp.int('short', 0, 0o666, '')
	assert 7 == fp.int('shorter', 0, 0o666, '')
}

fn test_beaware_for_argument_names_with_same_prefix_inverse() {
	mut fp := flag.new_flag_parser([
		'--shorter=7',
		'--short',
		'5',
	])
	assert 5 == fp.int('short', 0, 0o666, '')
	assert 7 == fp.int('shorter', 0, 0o666, '')
}

fn test_allow_to_skip_executable_path() {
	mut fp := flag.new_flag_parser(['./path/to/execuable'])
	fp.skip_executable()
	args := fp.finalize() or {
		assert false
		return
	}
	assert !args.contains('./path/to/execuable')
}

fn test_none_flag_arguments_are_allowed() {
	mut fp := flag.new_flag_parser([
		'file1',
		'--an_int=2',
		'file2',
		'file3',
		'--bool_without',
		'file4',
		'--outfile',
		'outfile',
	])
	assert 2 == fp.int('an_int', 0, 0o666, '')
	assert 'outfile' == fp.string('outfile', 0, 'bad', '')
	assert true == fp.bool('bool_without', 0, false, '')
}

fn test_finalize_returns_none_flag_arguments_ordered() {
	mut fp := flag.new_flag_parser(['d', 'b', 'x', 'a', '--outfile', 'outfile'])
	fp.string('outfile', 0, 'bad', '')
	finalized := fp.finalize() or {
		assert false
		return
	}
	expected := ['d', 'b', 'x', 'a']
	for i, v in finalized {
		assert v == expected[i]
	}
}

fn test_finalize_returns_error_for_unknown_flags() {
	mut fp := flag.new_flag_parser(['--known', '--unknown'])
	fp.bool('known', 0, false, '')
	finalized := fp.finalize() or {
		assert err.msg == "Unknown argument 'unknown'"
		return
	}
	assert finalized.len < 0 // expect error to be returned
}

fn test_allow_to_build_usage_message() {
	mut fp := flag.new_flag_parser([])
	fp.limit_free_args(1, 4)
	fp.application('flag_tool')
	fp.version('v0.0.0')
	fp.description('some short information about this tool')
	fp.int('an_int', 0, 0o666, 'some int to define')
	fp.bool('a_bool', 0, false, 'some bool to define')
	fp.bool('bool_without_but_really_big', 0, false, 'this should appear on the next line')
	fp.float('a_float', 0, 1.0, 'some float as well')
	fp.string('a_string', 0, 'not_stuff', 'your credit card number')
	usage := fp.usage()
	mut all_strings_found := true
	for s in ['flag_tool', 'v0.0.0', 'an_int <int>', 'a_bool', 'bool_without', 'a_float <float>',
		'a_string <string>', 'some int to define', 'some bool to define', 'this should appear on the next line',
		'some float as well', 'your credit card number', 'The arguments should be at least 1 and at most 4 in number.',
		'Usage', 'Options:', 'Description:', 'some short information about this tool'] {
		if !usage.contains(s) {
			eprintln(" missing '$s' in usage message")
			all_strings_found = false
		}
	}
	assert all_strings_found
}

fn test_if_no_description_given_usage_message_does_not_contain_descpription() {
	mut fp := flag.new_flag_parser([])
	fp.application('flag_tool')
	fp.version('v0.0.0')
	fp.bool('a_bool', 0, false, '')
	assert !fp.usage().contains('Description:')
}

fn test_if_no_options_given_usage_message_does_not_contain_options() {
	mut fp := flag.new_flag_parser([])
	fp.application('flag_tool')
	fp.version('v0.0.0')
	assert !fp.usage().contains('Options:')
}

fn test_free_args_could_be_limited() {
	mut fp1 := flag.new_flag_parser(['a', 'b', 'c'])
	fp1.limit_free_args(1, 4)
	args := fp1.finalize() or {
		assert false
		return
	}
	assert args[0] == 'a'
	assert args[1] == 'b'
	assert args[2] == 'c'
}

fn test_error_for_to_few_free_args() {
	mut fp1 := flag.new_flag_parser(['a', 'b', 'c'])
	fp1.limit_free_args(5, 6)
	args := fp1.finalize() or {
		assert err.msg.starts_with('Expected at least 5 arguments')
		return
	}
	assert args.len < 0 // expect an error and need to use args
}

fn test_error_for_to_much_free_args() {
	mut fp1 := flag.new_flag_parser(['a', 'b', 'c'])
	fp1.limit_free_args(1, 2)
	args := fp1.finalize() or {
		assert err.msg.starts_with('Expected at most 2 arguments')
		return
	}
	assert args.len < 0 // expect an error and need to use args
}

fn test_could_expect_no_free_args() {
	mut fp1 := flag.new_flag_parser(['a'])
	fp1.limit_free_args(0, 0)
	args := fp1.finalize() or {
		assert err.msg.starts_with('Expected no arguments')
		return
	}
	assert args.len < 0 // expect an error and need to use args
}

fn test_allow_abreviations() {
	mut fp := flag.new_flag_parser(['-v', '-o', 'some_file', '-i', '42', '-f', '2.0'])
	v := fp.bool('version', `v`, false, '')
	o := fp.string('output', `o`, 'empty', '')
	i := fp.int('count', `i`, 0, '')
	f := fp.float('value', `f`, 0.0, '')
	assert v == true
	assert o == 'some_file'
	assert i == 42
	assert f == 2.0
	u := fp.usage()
	assert u.contains(' -v')
	assert u.contains(' -o')
	assert u.contains(' -i')
	assert u.contains(' -f')
	assert u.contains('  -o, --output <string>')
	assert u.contains('  -i, --count <int>')
	assert u.contains('  -f, --value <float>')
}

fn test_allow_kebab_options() {
	default_value := 'this_is_the_default_value_of_long_option'
	long_option_value := 'this_is_a_long_option_value_as_argument'
	mut fp := flag.new_flag_parser(['--my-long-flag', 'true', '--my-long-option', long_option_value])
	my_flag := fp.bool('my-long-flag', 0, false, 'flag with long-kebab-name')
	my_option := fp.string('my-long-option', 0, default_value, 'string with long-kebab-name')
	assert my_flag == true
	assert my_option == long_option_value
	u := fp.usage()
	assert u.contains(' --my-long-flag')
	assert u.contains(' --my-long-option')
}

fn test_not_provided_option_is_not_returned() {
	mut fp := flag.new_flag_parser([])
	fp.bool_opt('some-flag', `a`, '') or {
		fp.int_opt('some-flag', `a`, '') or {
			fp.float_opt('some-flag', `a`, '') or {
				fp.string_opt('some-flag', `a`, '') or {
					// Everything should not return
					return
				}
				return
			}
			return
		}
		return
	}
	// If we reach here, one of them returned a value.
	assert false
}

fn test_provided_option_is_returned() {
	mut fp := flag.new_flag_parser(['-a', '-b', '3', '-c', 'hello', '-d', '3.14'])
	a := fp.bool_opt('some-flag', `a`, '') or { panic('bool_opt did not return a bool') }
	b := fp.int_opt('some-flag', `b`, '') or { panic('int_opt did not return an int') }
	c := fp.string_opt('some-flag', `c`, '') or { panic('string_opt did not return a string') }
	d := fp.float_opt('some-flag', `d`, '') or { panic('float_opt did not return a float') }
	assert true == a
	assert b == 3
	assert c == 'hello'
	assert d == 3.14
}

fn test_multiple_arguments() {
	mut fp := flag.new_flag_parser([
		'-a',
		'2',
		'-a',
		'3',
		'-a',
		'5',
		'-b',
		'a',
		'-b',
		'c',
		'-b',
		'b',
		'-c',
		'1.23',
		'-c',
		'2.34',
		'-c',
		'3.45',
	])
	// TODO Move to array comparison once it's implemented
	// assert fp.int_multi('some-flag', `a`, '') == [2, 3, 5] &&
	//	fp.string_multi('some-flag', `b`, '') == ['a', 'c', 'b'] &&
	//	fp.float_multi('some-flag', `c`, '') == [1.23, 2.34, 3.45]
	a := fp.int_multi('some-flag', `a`, '')
	b := fp.string_multi('some-flag', `b`, '')
	c := fp.float_multi('some-flag', `c`, '')
	assert a.len == 3
	assert b.len == 3
	assert c.len == 3
	assert a[0] == 2
	assert a[1] == 3
	assert a[2] == 5
	assert b[0] == 'a'
	assert b[1] == 'c'
	assert b[2] == 'b'
	assert c[0] == 1.23
	assert c[1] == 2.34
	assert c[2] == 3.45
}

fn test_long_options_that_start_with_the_same_letter_as_another_short_option() {
	mut fp := flag.new_flag_parser([
		'--vabc',
		'/abc',
	])
	verbose := fp.bool('verbose', `v`, false, 'Be more verbose.')
	vabc := fp.string('vabc', `x`, 'default', 'Another option that *may* conflict with v, but *should not*')
	assert verbose == false
	assert vabc == '/abc'
}

fn test_long_options_that_start_with_the_same_letter_as_another_short_option_both_set() {
	mut fp := flag.new_flag_parser([
		'-v',
		'--vabc',
		'/abc',
	])
	verbose := fp.bool('verbose', `v`, false, 'Be more verbose.')
	vabc := fp.string('vabc', `x`, 'default', 'Another option that *may* conflict with v, but *should not*')
	assert verbose == true
	assert vabc == '/abc'
}

fn test_single_dash() {
	mut fp := flag.new_flag_parser([
		'-',
	])
	flag_update := fp.bool('update', `u`, false, 'Update tools')
	assert flag_update == false
}

fn test_optional_flags() {
	mut fp := flag.new_flag_parser(['-a', '10', '-b'])
	fp.int_opt('some-flag', `a`, '') or {
		assert false
		return
	}
	b := fp.string_opt('another-flag', `b`, '') or { 'some_default_value' }
	assert b == 'some_default_value'
}
