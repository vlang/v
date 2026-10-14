import strconv

fn test_ftoa_long_64_uses_long_decimal_notation() {
	assert strconv.ftoa_long_64(34.2) == '34.2'
	assert strconv.ftoa_long_64(0.0) == '0.0'
	assert strconv.ftoa_long_64(-12.5) == '-12.5'
	assert strconv.ftoa_long_64(123.1234567891011121) == '123.12345678910111'
}
