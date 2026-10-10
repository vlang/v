import math
import strconv

struct SignedZeroCase {
	format string
	pos    string // expected for +0.0
	neg    string // expected for -0.0
}

// The expected strings are the output of C's `printf` for the same format and value.
const signed_zero_cases = [
	SignedZeroCase{'%e', '0.000000e+00', '-0.000000e+00'},
	SignedZeroCase{'%.0e', '0e+00', '-0e+00'},
	SignedZeroCase{'%.2e', '0.00e+00', '-0.00e+00'},
	SignedZeroCase{'%10.2e', '  0.00e+00', ' -0.00e+00'},
	SignedZeroCase{'%-10.2e', '0.00e+00  ', '-0.00e+00 '},
	SignedZeroCase{'%+e', '+0.000000e+00', '-0.000000e+00'},
	SignedZeroCase{'%+.2e', '+0.00e+00', '-0.00e+00'},
	SignedZeroCase{'%010.2e', '000.00e+00', '-00.00e+00'},
	SignedZeroCase{'%E', '0.000000E+00', '-0.000000E+00'},
	SignedZeroCase{'%.0E', '0E+00', '-0E+00'},
	SignedZeroCase{'%.2E', '0.00E+00', '-0.00E+00'},
	SignedZeroCase{'%10.2E', '  0.00E+00', ' -0.00E+00'},
	SignedZeroCase{'%-10.2E', '0.00E+00  ', '-0.00E+00 '},
	SignedZeroCase{'%+E', '+0.000000E+00', '-0.000000E+00'},
	SignedZeroCase{'%+.2E', '+0.00E+00', '-0.00E+00'},
	SignedZeroCase{'%010.2E', '000.00E+00', '-00.00E+00'},
	SignedZeroCase{'%f', '0.000000', '-0.000000'},
	SignedZeroCase{'%.0f', '0', '-0'},
	SignedZeroCase{'%.2f', '0.00', '-0.00'},
	SignedZeroCase{'%10.2f', '      0.00', '     -0.00'},
	SignedZeroCase{'%-10.2f', '0.00      ', '-0.00     '},
	SignedZeroCase{'%+f', '+0.000000', '-0.000000'},
	SignedZeroCase{'%+.2f', '+0.00', '-0.00'},
	SignedZeroCase{'%010.2f', '0000000.00', '-000000.00'},
	SignedZeroCase{'%F', '0.000000', '-0.000000'},
	SignedZeroCase{'%.0F', '0', '-0'},
	SignedZeroCase{'%.2F', '0.00', '-0.00'},
	SignedZeroCase{'%10.2F', '      0.00', '     -0.00'},
	SignedZeroCase{'%-10.2F', '0.00      ', '-0.00     '},
	SignedZeroCase{'%+F', '+0.000000', '-0.000000'},
	SignedZeroCase{'%+.2F', '+0.00', '-0.00'},
	SignedZeroCase{'%010.2F', '0000000.00', '-000000.00'},
	SignedZeroCase{'%g', '0', '-0'},
	SignedZeroCase{'%.0g', '0', '-0'},
	SignedZeroCase{'%.2g', '0', '-0'},
	SignedZeroCase{'%10.2g', '         0', '        -0'},
	SignedZeroCase{'%-10.2g', '0         ', '-0        '},
	SignedZeroCase{'%+g', '+0', '-0'},
	SignedZeroCase{'%+.2g', '+0', '-0'},
	SignedZeroCase{'%010.2g', '0000000000', '-000000000'},
	SignedZeroCase{'%G', '0', '-0'},
	SignedZeroCase{'%.0G', '0', '-0'},
	SignedZeroCase{'%.2G', '0', '-0'},
	SignedZeroCase{'%10.2G', '         0', '        -0'},
	SignedZeroCase{'%-10.2G', '0         ', '-0        '},
	SignedZeroCase{'%+G', '+0', '-0'},
	SignedZeroCase{'%+.2G', '+0', '-0'},
	SignedZeroCase{'%010.2G', '0000000000', '-000000000'},
]

fn test_v_sprintf_signed_zero() {
	pos_zero := 0.0
	neg_zero := -0.0
	assert !math.signbit(pos_zero)
	assert math.signbit(neg_zero)
	for c in signed_zero_cases {
		assert unsafe { strconv.v_sprintf(c.format, pos_zero) } == c.pos, '`${c.format}` of +0.0'
		assert unsafe { strconv.v_sprintf(c.format, neg_zero) } == c.neg, '`${c.format}` of -0.0'
	}
}

// sprintf3 formats `x` three times, for a format with three float verbs.
fn sprintf3(format string, x f64) string {
	return unsafe { strconv.v_sprintf(format, x, x, x) }
}

fn test_v_sprintf_negative_zero_from_other_sources() {
	from_bits := math.f64_from_bits(u64(0x8000_0000_0000_0000))
	assert sprintf3('%f %e %g', from_bits) == '-0.000000 -0.000000e+00 -0'
	assert sprintf3('[%5.1f|%-8.1e|%+g]', from_bits) == '[ -0.0|-0.0e+00|-0]'
	assert sprintf3('%.1f %.1e %g', math.copysign(0.0, -1.0)) == '-0.0 -0.0e+00 -0'
	// a negative product that underflows keeps its sign
	tiny := -1e-200
	underflow := tiny * 1e-200
	assert underflow == 0.0
	assert sprintf3('%f %e %g', underflow) == '-0.000000 -0.000000e+00 -0'
	// an f32 is promoted to f64 with its sign
	single := f32(-0.0)
	assert unsafe { strconv.v_sprintf('%.2f %.2e %g', single, single, single) } == '-0.00 -0.00e+00 -0'
}

fn test_v_sprintf_sign_of_other_values_is_unchanged() {
	assert sprintf3('%f %e %g', 1.5) == '1.500000 1.500000e+00 1.5'
	assert sprintf3('%f %e %g', -1.5) == '-1.500000 -1.500000e+00 -1.5'
	assert sprintf3('%+.1f %+.1e %+g', 1.5) == '+1.5 +1.5e+00 +1.5'
	assert sprintf3('%+.1f %+.1e %+g', -1.5) == '-1.5 -1.5e+00 -1.5'
	assert unsafe { strconv.v_sprintf('[%08.2f] [%010.2e]', -1.5, -1.5) } == '[-0001.50] [-01.50e+00]'
	// a negative value that rounds to zero keeps its sign too
	assert unsafe { strconv.v_sprintf('%.2f', -0.001) } == '-0.00'
	assert unsafe { strconv.v_sprintf('%.2f', 0.001) } == '0.00'
	// `%g` still switches to the scientific notation for small and large values
	assert unsafe { strconv.v_sprintf('%g %g', 1e-7, -1e-7) } == '1e-07 -1e-07'
	assert unsafe { strconv.v_sprintf('%g %g', 1e10, -1e10) } == '1e+10 -1e+10'
}

fn test_v_sprintf_nan_has_the_sign_of_its_sign_bit() {
	nan := math.nan()
	assert !math.signbit(nan)
	assert sprintf3('%f %e %g', nan) == 'nan nan nan'
}

fn test_format_fl_and_format_es_write_the_sign_passed_in_the_params() {
	neg_zero := -0.0
	assert strconv.format_fl(neg_zero, strconv.BF_param{ len1: 2, positive: false }) == '-0.00'
	assert strconv.format_es(neg_zero, strconv.BF_param{ len1: 2, positive: false }) == '-0.00e+00'
	assert strconv.format_fl_old(neg_zero, strconv.BF_param{ len1: 2, positive: false }) == '-0.00'
}
