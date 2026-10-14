import time

fn test_duration_maximum_str() {
	maximum := time.Duration(9223372036854775807)
	assert maximum.str() == '2562047:47:16'
	assert time.infinite == maximum
	assert time.infinite.str() == maximum.str()
	assert time.Duration(9223372036854775806).str() == '2562047:47:16'
	assert time.Duration(0).str() == '0ns'
}
