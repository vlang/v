// vtest vflags: -w
// Deliberate deprecated-type coverage must also run in suites using -W.
module strconv

fn grouped[T Numeric](x T) string {
	return format_thousands(x, ',')
}

fn test_format_legacy_signed_integer() {
	assert format_thousands(isize(-1234567), ',') == '-1,234,567'
	assert grouped(isize(-1234567)) == '-1,234,567'
}
