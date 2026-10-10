import strconv
import math

// The expected values of these tests are the exact decimal values of the f64 numbers,
// as C's `printf("%.*f")` prints them.

const exact_1e300 = '1000000000000000052504760255204420248704468581108159154915854115511802457988908195786371375080447864043704443832883878176942523235360430575644792184786706982848387200926575803737830233794788090059368953234970799945081119038967640880074652742780142494579258788820056842838115669472196386865459400540160'

const exact_max_f64 = '179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368'

struct ExactInt {
	f      f64
	digits string
}

const exact_ints = [
	ExactInt{1e15, '1000000000000000'},
	ExactInt{1e16, '10000000000000000'},
	ExactInt{1e17, '100000000000000000'},
	ExactInt{1e18, '1000000000000000000'},
	ExactInt{1e19, '10000000000000000000'},
	ExactInt{1e20, '100000000000000000000'},
	ExactInt{1e21, '1000000000000000000000'},
	ExactInt{1e22, '10000000000000000000000'},
	ExactInt{1e23, '99999999999999991611392'},
	ExactInt{1e24, '999999999999999983222784'},
	ExactInt{1e25, '10000000000000000905969664'},
	ExactInt{1e300, exact_1e300},
	ExactInt{math.max_f64, exact_max_f64},
	// 2^52, 2^52 + 1, 2^53 - 1, 2^53, 2^55, 2^63, 2^64
	ExactInt{4503599627370496.0, '4503599627370496'},
	ExactInt{4503599627370497.0, '4503599627370497'},
	ExactInt{9007199254740991.0, '9007199254740991'},
	ExactInt{9007199254740992.0, '9007199254740992'},
	ExactInt{36028797018963968.0, '36028797018963968'},
	ExactInt{9223372036854775808.0, '9223372036854775808'},
	ExactInt{18446744073709551616.0, '18446744073709551616'},
	ExactInt{123456789012345678901234567890.0, '123456789012345677877719597056'},
	// an f32 has the same value as an f64
	ExactInt{f64(f32(1e20)), '100000002004087734272'},
]

fn test_v_sprintf_f_of_a_large_float_has_all_the_digits() {
	assert unsafe { strconv.v_sprintf('%f', 1e23) } == '99999999999999991611392.000000'
	assert unsafe { strconv.v_sprintf('%f', -1e23) } == '-99999999999999991611392.000000'
	assert unsafe { strconv.v_sprintf('%f', 1e300) } == exact_1e300 + '.000000'
	assert unsafe { strconv.v_sprintf('%F', 1e300) } == exact_1e300 + '.000000'
	assert unsafe { strconv.v_sprintf('%.0f|%.2f', 1e23, -1e24) } == '99999999999999991611392|-999999999999999983222784.00'
}

fn test_fixed_notation_of_exact_integers() {
	for x in exact_ints {
		assert strconv.f64_to_str_lnd1(x.f, 0) == x.digits
		assert strconv.f64_to_str_lnd1(x.f, 2) == x.digits + '.00'
		assert strconv.f64_to_str_lnd1(x.f, 6) == x.digits + '.000000'
		for neg in [false, true] {
			f := if neg { -x.f } else { x.f }
			sign := if neg { '-' } else { '' }
			// format_fl and format_fl_old take the sign from their parameters
			p0 := strconv.BF_param{
				len1:     0
				positive: !neg
			}
			p6 := strconv.BF_param{
				len1:     6
				positive: !neg
			}
			assert strconv.format_fl(f, p0) == sign + x.digits
			assert strconv.format_fl(f, p6) == sign + x.digits + '.000000'
			assert strconv.format_fl_old(f, p0) == sign + x.digits
			assert strconv.format_fl_old(f, p6) == sign + x.digits + '.000000'
			assert unsafe { strconv.v_sprintf('%.0f', f) } == sign + x.digits
			assert unsafe { strconv.v_sprintf('%.2f', f) } == sign + x.digits + '.00'
			assert unsafe { strconv.v_sprintf('%f', f) } == sign + x.digits + '.000000'
		}
	}
	assert strconv.f64_to_str_lnd1(-1e23, 2) == '-99999999999999991611392.00'
	assert strconv.f64_to_str_lnd1(1e23, 40) == '99999999999999991611392.' + '0'.repeat(40)
	assert strconv.f64_to_str_lnd1(math.max_f64, 400) == exact_max_f64 + '.' + '0'.repeat(400)
}

fn test_format_fl_pads_an_exact_integer() {
	assert strconv.format_fl(1e23, strconv.BF_param{ len0: 30, len1: 2 }) == '    99999999999999991611392.00'
	assert strconv.format_fl(1e23, strconv.BF_param{ len0: 30, len1: 2, align: .left }) == '99999999999999991611392.00    '
	assert strconv.format_fl(-1e23, strconv.BF_param{
		len0:     30
		len1:     2
		pad_ch:   `0`
		positive: false
	}) == '-00099999999999999991611392.00'
	assert strconv.format_fl(1e23, strconv.BF_param{ len1: 1, sign_flag: true }) == '+99999999999999991611392.0'
	assert strconv.format_fl_old(1e23, strconv.BF_param{ len0: 30, len1: 2 }) == '    99999999999999991611392.00'
}

// dec_double doubles the decimal number in `digits`, that has its last digit first.
fn dec_double(mut digits []u8) {
	mut carry := u8(0)
	for i in 0 .. digits.len {
		d := digits[i] * 2 + carry
		digits[i] = d % 10
		carry = d / 10
	}
	if carry > 0 {
		digits << carry
	}
}

fn dec_str(digits []u8) string {
	mut s := []u8{cap: digits.len}
	for i := digits.len - 1; i >= 0; i-- {
		s << digits[i] + `0`
	}
	return s.bytestr()
}

// Every f64 from 2^52 on is `mant * 2^shift`, with a `mant` of 53 bits and a `shift` of 0 to 971.
// The expected digits come from doubling the decimal digits of `mant`, `shift` times.
fn test_exact_integers_of_every_exponent() {
	for fraction in [u64(0), 0xfffffffffffff, 0x5555555555555, 0x999999999999a, 0x123456789abcd,
		0x8000000000001] {
		mant := (u64(1) << 52) | fraction
		mut digits := []u8{}
		for m := mant; m > 0; m /= 10 {
			digits << u8(m % 10)
		}
		for shift in 0 .. 972 {
			f := math.f64_from_bits((u64(1075 + shift) << 52) | fraction)
			expected := dec_str(digits)
			assert strconv.f64_to_str_lnd1(f, 0) == expected, 'fraction: ${fraction:x}, shift: ${shift}'
			if shift % 97 == 0 {
				assert unsafe { strconv.v_sprintf('%.3f', -f) } == '-' + expected + '.000'
			}
			dec_double(mut digits)
		}
	}
}

fn test_precision_0_of_a_float_that_rounds_to_a_multiple_of_100() {
	assert strconv.f64_to_str_lnd1(99.5, 0) == '100'
	assert strconv.f64_to_str_lnd1(199.5, 0) == '200'
	assert strconv.f64_to_str_lnd1(999.5, 0) == '1000'
	assert strconv.f64_to_str_lnd1(99999.5, 0) == '100000'
	assert unsafe { strconv.v_sprintf('%.0f', 99.5) } == '100'
	assert unsafe { strconv.v_sprintf('%.0f', -999.5) } == '-1000'
	assert strconv.format_fl(99.5, strconv.BF_param{ len1: 0 }) == '100'
	// these were right already
	assert strconv.f64_to_str_lnd1(9.5, 0) == '10'
	assert strconv.f64_to_str_lnd1(239.5, 0) == '240'
	assert strconv.f64_to_str_lnd1(100.0, 0) == '100'
}

// Below 2^52 the digits and the rounding are the ones that they were.
fn test_floats_below_2_pow_52_are_unchanged() {
	assert strconv.f64_to_str_lnd1(4503599627370495.5, 1) == '4503599627370495.5'
	assert strconv.f64_to_str_lnd1(4503599627370495.5, 0) == '4503599627370496'
	assert strconv.f64_to_str_lnd1(0.125, 2) == '0.13'
	assert unsafe { strconv.v_sprintf('%f|%.2f', 123456.789, 1e15) } == '123456.789000|1000000000000000.00'
}
