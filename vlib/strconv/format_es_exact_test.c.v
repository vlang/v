module strconv

import math

fn test_format_es_exact_subnormal_precision() {
	value := math.f64_from_bits(u64(1))
	assert format_es(value, BF_param{ len1: 0 }) == '5e-324'
	assert format_es(value, BF_param{ len1: 2 }) == '4.94e-324'
	assert format_es(value, BF_param{ len1: 6 }) == '4.940656e-324'
	assert format_es(value, BF_param{ len1: 16 }) == '4.9406564584124654e-324'
	assert format_es(-value, BF_param{ len1: 2, positive: false }) == '-4.94e-324'
}

fn test_format_es_signed_zero_and_precision() {
	assert format_es(0.0, BF_param{ len1: 2 }) == '0.00e+00'
	assert format_es(-0.0, BF_param{ len1: 2, positive: false }) == '-0.00e+00'
	assert format_es(0.0, BF_param{ len1: 0 }) == '0e+00'
	assert format_es(-0.0, BF_param{ len1: 0, positive: false }) == '-0e+00'
	assert format_es(0.1, BF_param{ len1: 20 }) == '1.00000000000000005551e-01'
	assert format_es(9.95, BF_param{ len1: 1 }) == '9.9e+00'
	assert format_es(1.25, BF_param{ len1: 1 }) == '1.3e+00'
	assert format_es(1.75, BF_param{ len1: 1 }) == '1.8e+00'
}
