module strconv

fn test_add_thousands_sep_basic() {
	assert add_thousands_sep('1', Separator{}) == '1'
	assert add_thousands_sep('12', Separator{}) == '12'
	assert add_thousands_sep('123', Separator{}) == '123'
	assert add_thousands_sep('1234', Separator{}) == '1 234'
	assert add_thousands_sep('1234567', Separator{}) == '1 234 567'
	assert add_thousands_sep('12345678', Separator{}) == '12 345 678'
	assert add_thousands_sep('123456789', Separator{}) == '123 456 789'
}

fn test_add_thousands_sep_sign() {
	assert add_thousands_sep('-1234567', Separator{}) == '-1 234 567'
	assert add_thousands_sep('-100', Separator{}) == '-100'
	assert add_thousands_sep('-1000', Separator{}) == '-1 000'
	assert add_thousands_sep('+1234567', Separator{}) == '+1 234 567'
}

fn test_add_thousands_sep_decimal() {
	assert add_thousands_sep('1234567.891', Separator{}) == '1 234 567.891'
	assert add_thousands_sep('-1234567.89', Separator{ integer: ',' }) == '-1,234,567.89'
	assert add_thousands_sep('1000.0', Separator{}) == '1 000.0'
	assert add_thousands_sep('12.5', Separator{}) == '12.5'
	assert add_thousands_sep('1234567.89', Separator{
		integer: '.'
		decimal: ','
	}) == '1.234.567,89'
}

fn test_add_thousands_sep_exponent() {
	assert add_thousands_sep('1e+06', ' ') == '1e+06'
	assert add_thousands_sep('-1234567.89E-03', Separator{
		integer: ','
		decimal: ','
	}) == '-1,234,567,89E-03'
}

fn test_add_thousands_sep_custom_sep() {
	assert add_thousands_sep('1000000', ',') == '1,000,000'
	assert add_thousands_sep('1000000', '.') == '1.000.000'
	assert add_thousands_sep('1000000', '_') == '1_000_000'
}

fn test_add_thousands_sep_edge_cases() {
	assert add_thousands_sep('', Separator{}) == ''
	assert add_thousands_sep('0', Separator{}) == '0'
	assert add_thousands_sep('7', Separator{}) == '7'
	assert add_thousands_sep('1234', '') == '1234'
}

fn test_format_thousands() {
	assert format_thousands(1234567, Separator{}) == '1 234 567'
	assert format_thousands(-1234567, Separator{ integer: ',' }) == '-1,234,567'
	assert format_thousands(100, Separator{}) == '100'
	assert format_thousands(0, Separator{}) == '0'
	assert format_thousands(i64(-9223372036854775807 - 1), Separator{}) == '-9 223 372 036 854 775 808'
	assert format_thousands(f32(1234567.5), Separator{}) == '1 234 567.5'
	assert format_thousands(f64(1234567.89), Separator{
		integer: '.'
		decimal: ','
	}) == '1.234.567,89'
}
