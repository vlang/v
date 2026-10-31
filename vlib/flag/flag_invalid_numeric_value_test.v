import flag

fn finalize_error(mut fp flag.FlagParser) string {
	rest := fp.finalize() or { return err.msg() }
	return 'no error, rest: ${rest}'
}

fn test_int_flag_with_a_non_integer_value_fails_in_finalize() {
	mut fp := flag.new_flag_parser(['flg', '--n', 'abc'])
	fp.skip_executable()
	n := fp.int('n', `n`, 7, '')
	assert n == 7
	assert finalize_error(mut fp) == 'Invalid value `abc` for flag `--n`: expected an integer'
}

fn test_int_flag_with_trailing_garbage_fails_in_finalize() {
	mut fp := flag.new_flag_parser(['-n', '42abc'])
	fp.int('n', `n`, 0, '')
	assert finalize_error(mut fp) == 'Invalid value `42abc` for flag `--n`: expected an integer'
}

fn test_int_flag_that_overflows_fails_in_finalize() {
	mut fp := flag.new_flag_parser(['--n=99999999999999999999'])
	n := fp.int('n', `n`, 0, '')
	assert n == 0
	assert finalize_error(mut fp) == 'Invalid value `99999999999999999999` for flag `--n`: out of range for int'
}

fn test_int_flag_accepts_the_whole_int_range() {
	max := max_int.str()
	min := min_int.str()
	mut fp := flag.new_flag_parser(['--max', max, '--min=${min}'])
	assert fp.int('max', 0, 0, '') == max_int
	assert fp.int('min', 0, 0, '') == min_int
	assert fp.finalize()! == []
	// one past the largest `int` (64 bits on 64-bit targets), is out of range
	too_big := (u64(max_int) + 1).str()
	mut fp2 := flag.new_flag_parser(['--n', too_big])
	fp2.int('n', 0, 0, '')
	assert finalize_error(mut fp2) == 'Invalid value `${too_big}` for flag `--n`: out of range for int'
}

fn test_int_flag_still_accepts_what_string_int_accepts() {
	mut fp := flag.new_flag_parser(['-a', '42', '--b=-7', '--c', '0x10', '--d=1_000', '--e', '010',
		'--f=+3', '--g', '0b101', '--h=0o17'])
	assert fp.int('a_int', `a`, 0, '') == 42
	assert fp.int('b', 0, 0, '') == -7
	assert fp.int('c', 0, 0, '') == 16
	assert fp.int('d', 0, 0, '') == 1000
	assert fp.int('e', 0, 0, '') == 10
	assert fp.int('f', 0, 0, '') == 3
	assert fp.int('g', 0, 0, '') == 5
	assert fp.int('h', 0, 0, '') == 15
	assert fp.finalize()! == []
}

fn test_int_opt_returns_the_error_and_finalize_fails_too() {
	mut fp := flag.new_flag_parser(['--n', 'abc'])
	if n := fp.int_opt('n', 0, '') {
		assert false, 'expected an error, got ${n}'
	} else {
		assert err.msg() == 'Invalid value `abc` for flag `--n`: expected an integer'
	}
	assert finalize_error(mut fp) == 'Invalid value `abc` for flag `--n`: expected an integer'
}

fn test_int_val_with_an_option_default_returns_none_and_finalize_fails() {
	mut fp := flag.new_flag_parser(['--n', 'abc'])
	assert fp.int_val('n', 0, ?int(none), '') == none
	assert finalize_error(mut fp) == 'Invalid value `abc` for flag `--n`: expected an integer'
}

fn test_int_multi_leaves_out_invalid_values_and_finalize_fails() {
	mut fp := flag.new_flag_parser(['-n', '1', '-n', 'x', '-n', '3'])
	assert fp.int_multi('num', `n`, '') == [1, 3]
	assert finalize_error(mut fp) == 'Invalid value `x` for flag `--num`: expected an integer'
}

fn test_float_flag_with_a_non_number_value_fails_in_finalize() {
	mut fp := flag.new_flag_parser(['flg', '--f', 'xyz'])
	fp.skip_executable()
	f := fp.float('f', `f`, 1.5, '')
	assert f == 1.5
	assert finalize_error(mut fp) == 'Invalid value `xyz` for flag `--f`: expected a number'
}

fn test_float_flag_with_trailing_garbage_or_overflow_fails_in_finalize() {
	mut fp := flag.new_flag_parser(['--f=2.5x'])
	fp.float('f', 0, 0.0, '')
	assert finalize_error(mut fp) == 'Invalid value `2.5x` for flag `--f`: expected a number'
	mut fp2 := flag.new_flag_parser(['--f=1e400'])
	fp2.float('f', 0, 0.0, '')
	assert finalize_error(mut fp2) == 'Invalid value `1e400` for flag `--f`: out of range for f64'
}

fn test_float_flags_still_accept_valid_numbers() {
	mut fp := flag.new_flag_parser(['--a=2.5', '--b', '-1e3', '-c', '.5', '--d', '7'])
	assert fp.float('a', 0, 0.0, '') == 2.5
	assert fp.float('b', 0, 0.0, '') == -1000.0
	assert fp.float('c_float', `c`, 0.0, '') == 0.5
	assert fp.float_opt('d', 0, '')! == 7.0
	assert fp.finalize()! == []
}

fn test_float_opt_and_float_multi_report_invalid_values() {
	mut fp := flag.new_flag_parser(['--f', 'nope', '-m', '1.5', '-m', 'bad'])
	if f := fp.float_opt('f', 0, '') {
		assert false, 'expected an error, got ${f}'
	} else {
		assert err.msg() == 'Invalid value `nope` for flag `--f`: expected a number'
	}
	assert fp.float_multi('multi', `m`, '') == [1.5]
	// the first invalid value is reported
	assert finalize_error(mut fp) == 'Invalid value `nope` for flag `--f`: expected a number'
}

fn test_bool_flag_with_a_value_other_than_true_or_false_fails_in_finalize() {
	mut fp := flag.new_flag_parser(['--b=yes'])
	assert fp.bool('b', 0, false, '') == false
	assert finalize_error(mut fp) == 'Invalid value `yes` for flag `--b`: expected `true` or `false`'
	mut fp2 := flag.new_flag_parser(['--b=false', '--c=true', '--d', 'false'])
	assert fp2.bool('b', 0, true, '') == false
	assert fp2.bool('c', 0, false, '') == true
	assert fp2.bool('d', 0, true, '') == false
	assert fp2.finalize()! == []
}

struct NumericConfig {
	n   int
	i8  i8
	u8  u8
	u64 u64
	f   f64
	f32 f32
	ns  []int
	fs  []f32
}

fn to_struct_error(args []string) string {
	c, _ := flag.to_struct[NumericConfig](args) or { return err.msg() }
	return 'no error: ${c}'
}

fn test_to_struct_rejects_out_of_range_and_non_number_values() {
	assert to_struct_error(['--n=99999999999999999999']) == 'can not assign out of range value `99999999999999999999` from flag `--n=99999999999999999999` to `NumericConfig.n`'
	assert to_struct_error(['--i8', '128']) == 'can not assign out of range value `128` from flag `--i8` to `NumericConfig.i8`'
	assert to_struct_error(['--u8', '256']) == 'can not assign out of range value `256` from flag `--u8` to `NumericConfig.u8`'
	assert to_struct_error(['--u64', '-1']) == 'can not assign out of range value `-1` from flag `--u64` to `NumericConfig.u64`'
	assert to_struct_error(['--f', 'xyz']) == 'can not assign non-number value `xyz` from flag `--f` to `NumericConfig.f`'
	assert to_struct_error(['--f', '1e400']) == 'can not assign out of range value `1e400` from flag `--f` to `NumericConfig.f`'
	assert to_struct_error(['--f32', '1e39']) == 'can not assign out of range value `1e39` from flag `--f32` to `NumericConfig.f32`'
	assert to_struct_error(['--ns', '1', '--ns', 'x']) == 'can not assign non-integer value `x` from flag `--ns` to `NumericConfig.ns`'
	assert to_struct_error(['--fs', 'x']) == 'can not assign non-number value `x` from flag `--fs` to `NumericConfig.fs`'
}

fn test_to_struct_accepts_values_that_fit() {
	c, rest := flag.to_struct[NumericConfig](['--n', max_int.str(), '--i8=-128', '--u8', '+255',
		'--u64', '18446744073709551615', '--f', '2.5', '--f32', '-3e38', '--ns', '1', '--ns', '-2',
		'--ns', '0x10', '--ns', '1_000', '--fs', 'inf'])!
	assert rest == []
	assert c.n == max_int
	assert c.i8 == -128
	assert c.u8 == 255
	assert c.u64 == max_u64
	assert c.f == 2.5
	assert c.f32 == f32(-3e38)
	assert c.ns == [1, -2, 16, 1000]
	assert c.fs.len == 1 && c.fs[0] > max_f32_value
}

const max_f32_value = f32(3.40282346638528859811704183484516925440e+38)
