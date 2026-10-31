import time

fn test_parse_format_meridiem_hours() {
	for hour in 1 .. 13 {
		for marker in ['AM', 'PM', 'am', 'pm'] {
			token := if marker in ['AM', 'PM'] { 'A' } else { 'a' }
			expected := hour % 12 + if marker in ['PM', 'pm'] { 12 } else { 0 }
			for hour_token in ['h', 'hh'] {
				input_hour := if hour_token == 'hh' { '${hour:02}' } else { hour.str() }
				parsed := time.parse_format('${input_hour}:30:45${marker}', '${hour_token}:mm:ss${token}')!
				assert parsed.hour == expected && parsed.minute == 30 && parsed.second == 45
			}
		}
	}
}

fn test_parse_format_meridiem_before_hour() {
	assert time.parse_format('PM 02:30', 'A hh:mm')!.hour == 14
	assert time.parse_format('am 12:00', 'a hh:mm')!.hour == 0
}

fn test_parse_format_rejects_invalid_meridiem() {
	for input in ['12:00:00A', '12:00:00XM', '00:00:00AM', '13:00:00PM', '12:00:00am'] {
		time.parse_format(input, 'hh:mm:ssA') or { continue }
		assert false, 'invalid meridiem time ${input} must be rejected'
	}
	time.parse_format('12:00:00AM', 'hh:mm:ssa') or { return }
	assert false, 'lowercase meridiem requires lowercase input'
}

fn test_parse_format_preserves_unmarked_hours() {
	assert time.parse_format('00:00', 'hh:mm')!.hour == 0
	assert time.parse_format('23:30', 'hh:mm')!.hour == 23
}
