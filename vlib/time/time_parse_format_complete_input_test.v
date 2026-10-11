import time

fn test_parse_format_rejects_trailing_input() {
	for suffix in ['xyz', ' ', '\n', '0'] {
		time.parse_format('2024-07-15${suffix}', 'YYYY-MM-DD') or {
			assert err.msg().contains('extra text: ${suffix}')
			continue
		}
		assert false, 'trailing input ${suffix} must be rejected'
	}
}

fn test_parse_format_consumes_literals_and_complete_input() {
	t := time.parse_format('2024-07-15--', 'YYYY-MM-DD--')!
	assert t.year == 2024 && t.month == 7 && t.day == 15
	assert time.parse_format('2024', 'YYYY')!.year == 2024
	time.parse_format('2024', '') or {
		assert err.msg().contains('extra text: 2024')
		return
	}
	assert false, 'an empty format cannot consume nonempty input'
}
