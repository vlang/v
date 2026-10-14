import time

fn test_parse_unpadded_clock_round_trip() {
	for clock in [[0, 0, 0], [3, 4, 5], [9, 9, 9], [10, 10, 10], [23, 40, 50], [23, 59, 59]] {
		value := time.new(
			year:   2024
			month:  7
			day:    15
			hour:   clock[0]
			minute: clock[1]
			second: clock[2]
		)
		for layout in ['YYYY-MM-DD k:m:s', 'YYYY-MM-DD H:m:s', 'YYYY-MM-DD h:m:sA', 'YYYY-MM-DD k:mm:ss',
			'YYYY-MM-DD HH:m:ss', 'YYYY-MM-DD HH:mm:s'] {
			// custom_format uses 24 at midnight for k, while parse_format documents 0..23.
			if clock[0] == 0 && layout.contains('k') {
				continue
			}
			encoded := value.custom_format(layout)
			decoded := time.parse_format(encoded, layout)!
			assert decoded == value, '${layout}: ${encoded}'
		}
	}
}

fn test_unpadded_clock_accepts_leading_zero() {
	value := time.parse_format('2024-07-15 03:04:05', 'YYYY-MM-DD k:m:s')!
	assert value.hour == 3 && value.minute == 4 && value.second == 5
	zero := time.parse_format('2024-07-15 0:0:0', 'YYYY-MM-DD k:m:s')!
	assert zero.hour == 0 && zero.minute == 0 && zero.second == 0
}

fn test_unpadded_clock_rejects_invalid_values() {
	for input in ['24:0:0', '0:60:0', '0:0:60', '123:0:0', '0:123:0', '0:0:123', ':0:0', '0::0',
		'0:0:', 'x:0:0', '0:x:0', '0:0:x'] {
		if value := time.parse_format(input, 'k:m:s') {
			assert false, '${input}: ${value}'
		}
	}
	for layout in ['kk:m:s', 'k:mm:s', 'k:m:ss'] {
		if value := time.parse_format('3:4:5', layout) {
			assert false, '${layout}: ${value}'
		}
	}
}
