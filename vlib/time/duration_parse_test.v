import time

// The expected values are in nanoseconds. They are what Go's `time.ParseDuration` returns
// for the same strings.
const accepted = {
	// the strings from https://github.com/vlang/v/issues/29938
	'1h':                                          i64(3_600_000_000_000)
	'1h30m':                                       5_400_000_000_000
	'1.5h':                                        5_400_000_000_000
	'-5m':                                         -300_000_000_000
	'+3s':                                         3_000_000_000
	'300ms':                                       300_000_000
	'1µs':                                         1_000
	'1us':                                         1_000
	'1ns':                                         1
	'1.000000001s':                                1_000_000_001
	'.5s':                                         500_000_000
	'1.s':                                         1_000_000_000
	'+0':                                          0
	'-0':                                          0
	'0':                                           0
	'0s':                                          0
	'0.0s':                                        0
	'-0s':                                         0
	// all the units; the micro sign U+00B5, and the Greek small letter mu U+03BC look the same
	'10ns':                                        10
	'11us':                                        11_000
	'12\u00b5s':                                   12_000
	'12\u03bcs':                                   12_000
	'13ms':                                        13_000_000
	'14s':                                         14_000_000_000
	'15m':                                         900_000_000_000
	'16h':                                         57_600_000_000_000
	// signs
	'+5s':                                         5_000_000_000
	'-5s':                                         -5_000_000_000
	'+1h30m':                                      5_400_000_000_000
	'-1h30m':                                      -5_400_000_000_000
	// fractions; what is less than a nanosecond is cut off
	'5.0s':                                        5_000_000_000
	'5.6s':                                        5_600_000_000
	'5.s':                                         5_000_000_000
	'1.004s':                                      1_004_000_000
	'1.0040s':                                     1_004_000_000
	'100.00100s':                                  100_001_000_000
	'0.5h':                                        1_800_000_000_000
	'0.25m':                                       15_000_000_000
	'1.5ms':                                       1_500_000
	'1.5us':                                       1_500
	'1.5ns':                                       1
	'0.9ns':                                       0
	'.001us':                                      1
	'0.000000001s':                                1
	'0.0000000001s':                               0
	'1.999999999s':                                1_999_999_999
	'0.000000001h':                                3_600
	'-.5s':                                        -500_000_000
	'+.5s':                                        500_000_000
	'-0.000h':                                     0
	// several components, in a descending order, in an ascending order, and with repeated units
	'1h1m1.5s':                                    3_661_500_000_000
	'2h45m30.5s':                                  9_930_500_000_000
	'39h9m14.425s':                                140_954_425_000_000
	'-2m3.4s':                                     -123_400_000_000
	'10.5s4m':                                     250_500_000_000
	'1h2m3s4ms5us6ns':                             3_723_004_005_006
	'6ns5us4ms3s2m1h':                             3_723_004_005_006
	'1s1s':                                        2_000_000_000
	'1h1h1h':                                      10_800_000_000_000
	'1.5h1.5h':                                    10_800_000_000_000
	'1ms1\u00b5s1\u03bcs1us':                      1_003_000
	'0h0m0s':                                      0
	'000005s':                                     5_000_000_000
	// more fraction digits than the result has room for
	'0.3333333333333333333h':                      1_200_000_000_000
	'0.100000000000000000000h':                    360_000_000_000
	'1.0000000000000000000000000000001s':          1_000_000_000
	// 2^53 + 1, which an f64 can not hold
	'9007199254740993ns':                          9_007_199_254_740_993
	// the limits of the i64 range of nanoseconds
	'9223372036854775807ns':                       max_i64
	'+9223372036854775807ns':                      max_i64
	'-9223372036854775807ns':                      -max_i64
	'-9223372036854775808ns':                      min_i64
	'9223372036854775.807us':                      max_i64
	'-9223372036854775.808us':                     min_i64
	'9223372036s854ms775us807ns':                  max_i64
	'-9223372036s854ms775us808ns':                 min_i64
	'2562047h47m16.854775807s':                    max_i64
	'-2562047h47m16.854775808s':                   min_i64
	'4611686018427387904ns4611686018427387903ns':  max_i64
	'-4611686018427387904ns4611686018427387904ns': min_i64
	'-9223372036854775807ns1ns':                   min_i64
	'2562047h':                                    9_223_369_200_000_000_000
	'-2562047h':                                   -9_223_369_200_000_000_000
	'153722867m':                                  9_223_372_020_000_000_000
	'9223372036s':                                 9_223_372_036_000_000_000
	'9223372036854ms':                             9_223_372_036_854_000_000
	'9223372036854775us':                          9_223_372_036_854_775_000
}

// Strings that are not durations, and durations that do not fit in an i64 of nanoseconds.
const invalid = [
	'',
	'-',
	'+',
	'.',
	'-.',
	'+.',
	'.s',
	'-.s',
	'+.s',
	'..5s',
	's',
	'ms',
	'h1',
	'abc',
	'inf',
	'-inf',
	'--5s',
	'+-5s',
	'-+5s',
	'++5s',
	' ',
	' 1h',
	'\t1s',
	// one more than the largest duration, and one less than the smallest one
	'9223372036854775808ns',
	'+9223372036854775808ns',
	'-9223372036854775809ns',
	'9223372036854775.808us',
	'-9223372036854775.809us',
	'9223372036854ms775us808ns',
	'2562047h47m16.854775808s',
	'-2562047h47m16.854775809s',
	'9223372036854775807ns1ns',
	'-9223372036854775808ns1ns',
	'4611686018427387904ns4611686018427387904ns',
	// a single component that is too large for its unit
	'2562048h',
	'-2562048h',
	'2562047.99h',
	'9223373h',
	'153722868m',
	'9223372037s',
	'9223372036855ms',
	'9223372036854776us',
	// numbers that do not fit in 64 bits
	'18446744073709551615ns',
	'18446744073709551616ns',
	'100000000000000000000ns',
	'99999999999999999999h',
	// 2^63 ns + 2^63 ns must not wrap around to 0, which is what Go returns for these two
	'9223372036854775808ns9223372036854775808ns',
	'-9223372036854775808ns9223372036854775808ns',
]

// Numbers that are not followed by a unit. Only a single `0` can stand alone.
const without_unit = ['5', '1h30', '1h30m5', '1.5', '.5', '1.', '00', '+00', '0.', '0.0', '1.5.5s',
	'1..s']

// Strings with something else in the place of a unit, and what is reported as the unknown unit.
const with_unknown_unit = {
	'5x':       'x'
	'1S':       'S'
	'1e3s':     'e'
	'1E3s':     'E'
	'1e-3s':    'e-'
	'1d':       'd'
	'1w':       'w'
	'1y':       'y'
	'1H':       'H'
	'1M':       'M'
	'1MS':      'MS'
	'1Ms':      'Ms'
	'1mS':      'mS'
	'1min':     'min'
	'1sec':     'sec'
	'1hr':      'hr'
	'1hs':      'hs'
	'1\u00b5':  '\u00b5'
	'1\u03bc':  '\u03bc'
	'1\u00b5S': '\u00b5S'
	'0x10s':    'x'
	'1_000s':   '_'
	'1,5s':     ','
	'5s-':      's-'
	'1h-30m':   'h-'
	'1h+30m':   'h+'
	'1 s':      ' s'
	'1h 30m':   'h '
	'1h ':      'h '
	'1s\n':     's\n'
	// the forms that `Duration.str()` uses for a minute or more
	'2:33.015': ':'
	'1:30:00':  ':'
	'-1:30:00': ':'
}

// parse_result returns either the parsed nanoseconds, or the error message for `input`.
fn parse_result(input string) string {
	d := time.parse_duration(input) or { return err.msg() }
	return d.nanoseconds().str()
}

fn test_parse_duration_accepted() {
	for input, nanoseconds in accepted {
		assert parse_result(input) == nanoseconds.str(), 'input: `${input}`'
	}
	assert time.parse_duration('1h30m')! == time.hour + 30 * time.minute
	assert time.parse_duration('-1.5s')! == -1500 * time.millisecond
	assert time.parse_duration('2h45m30.5s')! == 2 * time.hour + 45 * time.minute +
		30 * time.second + 500 * time.millisecond
	assert time.parse_duration('-9223372036854775808ns')!.nanoseconds() == min_i64
	assert time.parse_duration('1\u00b5s')! == time.microsecond
	assert time.parse_duration('1\u03bcs')! == time.microsecond
}

fn test_parse_duration_invalid() {
	for input in invalid {
		assert parse_result(input) == 'invalid duration: "${input}"'
	}
}

fn test_parse_duration_without_unit() {
	for input in without_unit {
		assert parse_result(input) == 'missing unit in duration: "${input}"'
	}
}

fn test_parse_duration_with_unknown_unit() {
	for input, unit in with_unknown_unit {
		assert parse_result(input) == 'unknown unit "${unit}" in duration: "${input}"'
	}
}

// Below a minute, `Duration.str()` writes a number with a unit, which parse_duration reads back.
// It keeps 3 digits after the dot, so only durations without smaller parts round trip exactly.
fn test_parse_duration_of_duration_str() {
	for d in [time.Duration(0), time.nanosecond, 234 * time.nanosecond, 999 * time.nanosecond,
		time.microsecond, 7 * time.microsecond + 234 * time.nanosecond, 999_999 * time.nanosecond,
		time.millisecond, 15 * time.millisecond + 7 * time.microsecond, 999_999 * time.microsecond,
		time.second, 33 * time.second + 15 * time.millisecond, 59 * time.second +
			999 * time.millisecond] {
		assert time.parse_duration(d.str())! == d, 'd.str(): ${d.str()}'
		assert time.parse_duration((-d).str())! == -d, '(-d).str(): ${(-d).str()}'
	}
	assert time.Duration(time.second + 5 * time.microsecond).str() == '1.000s'
	assert time.parse_duration('1.000s')! == time.second
}
