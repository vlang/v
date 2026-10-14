module strconv

import math

fn test_format_fl_expands_large_exact_values() {
	assert format_fl(1e21, BF_param{ len1: 0 }) == '1000000000000000000000'
	assert format_fl(1e22, BF_param{ len1: 0 }) == '10000000000000000000000'
	value := math.f64_from_bits(u64(0x44b52d02c7e14af6))
	assert format_fl(value, BF_param{ len1: 0 }) == '99999999999999991611392'
	assert format_fl(value, BF_param{ len1: 6 }) == '99999999999999991611392.000000'
	assert format_fl(-value, BF_param{ len1: 2, positive: false }) == '-99999999999999991611392.00'
}

fn test_format_fl_exact_fractional_digits() {
	assert format_fl(0.1, BF_param{ len1: 20 }) == '0.10000000000000000555'
	assert format_fl(0.125, BF_param{ len1: 2 }) == '0.13'
	assert format_fl(0.375, BF_param{ len1: 2 }) == '0.38'
	assert format_fl(2.5, BF_param{ len1: 0 }) == '3'
	assert format_fl(3.5, BF_param{ len1: 0 }) == '4'
	assert format_fl(0.0, BF_param{ len1: 0 }) == '0'
	assert format_fl(0.0, BF_param{ len1: 3 }) == '0.000'
}

fn test_format_fl_largest_finite_value() {
	expected := '1797693134862315708145274237317043567980705675258449965989174768031572607800' +
		'2853876058955863276687817154045895351438246423432132688946418276846754670353' +
		'7516986049910576551282076245490090389328944075868508455133942304583236903222' +
		'9481658085593321233482747978262041447231687381771809192998812504040261841248' +
		'58368'
	assert expected.len == 309
	assert format_fl(math.max_f64, BF_param{ len1: 0 }) == expected
	assert format_fl(math.max_f64, BF_param{ len1: 2 }) == expected + '.00'
	assert format_fl(math.max_f64, BF_param{ len1: 250 }) == expected + '.' + '0'.repeat(250)
}
