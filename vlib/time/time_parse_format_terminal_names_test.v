import time

fn test_parse_format_terminal_month_names() {
	for i, month in time.long_months {
		assert time.parse_format(month, 'MMMM')!.month == i + 1
		assert time.parse_format(month[..3], 'MMM')!.month == i + 1
		assert time.parse_format('2024 ${month}', 'YYYY MMMM')!.month == i + 1
		assert time.parse_format('2024 ${month[..3]}', 'YYYY MMM')!.month == i + 1
	}
}

fn test_parse_format_terminal_weekday_names() {
	for day in time.long_days {
		for format, name in {
			'dddd': day
			'ddd':  day[..3]
			'dd':   day[..2]
		} {
			assert time.parse_format(name, format)!.day == 1
			parsed := time.parse_format('2024-07-15 ${name}', 'YYYY-MM-DD ${format}')!
			assert parsed.year == 2024 && parsed.month == 7 && parsed.day == 15
		}
	}
}

fn test_parse_format_rejects_incomplete_terminal_names() {
	for name in ['Ju', 'Jux', 'Januar'] {
		time.parse_format(name, 'MMMM') or { continue }
		assert false, 'invalid month name ${name} must be rejected'
	}
	for format, name in {
		'dddd': 'Monda'
		'ddd':  'Mo'
		'dd':   'M'
	} {
		time.parse_format(name, format) or { continue }
		assert false, 'incomplete weekday name ${name} must be rejected'
	}
}
