import time

fn test_custom_format_pads_four_digit_years() {
	for year, expected in {
		0:     '0000'
		1:     '0001'
		9:     '0009'
		99:    '0099'
		100:   '0100'
		999:   '0999'
		1000:  '1000'
		2024:  '2024'
		9999:  '9999'
		10000: '10000'
	} {
		t := time.Time{ year: year, month: 1, day: 1 }
		assert t.custom_format('YYYY') == expected
		assert t.custom_format('YYYY-MM-DD') == '${expected}-01-01'
	}
}

fn test_custom_format_pads_last_two_year_digits() {
	for year, expected in {
		0:     '00'
		1:     '01'
		99:    '99'
		100:   '00'
		999:   '99'
		2024:  '24'
		12345: '45'
	} {
		assert time.Time{ year: year }.custom_format('YY') == expected
	}
}

fn test_custom_format_small_year_roundtrip() {
	for year in [0, 1, 100, 999] {
		t := time.new(year: year, month: 1, day: 1)
		assert time.parse_format(t.custom_format('YYYY-MM-DD'), 'YYYY-MM-DD')!.year == year
	}
}

fn test_custom_format_preserves_negative_years() {
	for year, expected in {
		-1:     '-1|'
		-99:    '-99|9'
		-100:   '-100|00'
		-999:   '-999|99'
		-2024:  '-2024|02'
		-99999: '-99999|99'
	} {
		t := time.Time{ year: year }
		assert t.custom_format('YYYY|YY') == expected
		assert t.custom_format('N YYYY|YY') == 'AD ${expected}'
		assert t.custom_format('NN YYYY|YY') == 'Anno Domini ${expected}'
	}
}
