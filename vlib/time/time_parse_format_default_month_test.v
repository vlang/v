import time

fn test_parse_format_day_31_defaults_to_january() {
	for format in ['D', 'DD'] {
		parsed := time.parse_format('31', format)!
		assert parsed.month == 1 && parsed.day == 31
	}
	parsed := time.parse_format('2024 31', 'YYYY DD')!
	assert parsed.year == 2024 && parsed.month == 1 && parsed.day == 31
}

fn test_parse_format_day_31_rejects_explicit_short_months() {
	for month in [2, 4, 6, 9, 11] {
		time.parse_format('2024-${month:02}-31', 'YYYY-MM-DD') or { continue }
		assert false, 'month ${month} does not have 31 days'
	}
	for month in [1, 3, 5, 7, 8, 10, 12] {
		assert time.parse_format('2024-${month:02}-31', 'YYYY-MM-DD')!.day == 31
	}
}

fn test_parse_format_preserves_february_validation() {
	assert time.parse_format('2024-02-29', 'YYYY-MM-DD')!.day == 29
	time.parse_format('2023-02-29', 'YYYY-MM-DD') or { return }
	assert false, 'February 29 needs a leap year'
}
