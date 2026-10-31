import time

fn test_duration_minimum_str() {
	assert time.Duration(i64(-9223372036854775807) - 1).str() == '-2562047:47:16'
	assert time.Duration(-9223372036854775807).str() == '-2562047:47:16'
	assert time.Duration(-1).str() == '-1ns'
	assert time.Duration(-1001).str() == '-1.001us'
	assert time.Duration(-1000001).str() == '-1.000ms'
	assert time.Duration(-1000000001).str() == '-1.000s'
	assert time.Duration(-60001000000).str() == '-1:00.001'
	assert time.Duration(-3601000000000).str() == '-1:00:01'
	assert time.Duration(0).str() == '0ns'
}
