import time

fn test_parse() {
	s := '2018-01-27 12:48:34'
	t := time.parse(s) or {
		assert false
		return
	}
	assert t.year == 2018 &&
		t.month == 1 && t.day == 27 && t.hour == 12 && t.minute == 48 && t.second == 34
	assert t.unix == 1517057314
}

fn test_parse_invalid() {
	s := 'Invalid time string'
	time.parse(s) or {
		assert true
		return
	}
	assert false
}

fn test_parse_rfc2822() {
	s1 := 'Thu, 12 Dec 2019 06:07:45 GMT'
	t1 := time.parse_rfc2822(s1) or {
		assert false
		return
	}
	assert t1.year == 2019 &&
		t1.month == 12 && t1.day == 12 && t1.hour == 6 && t1.minute == 7 && t1.second == 45
	assert t1.unix == 1576130865
	s2 := 'Thu 12 Dec 2019 06:07:45 +0800'
	t2 := time.parse_rfc2822(s2) or {
		assert false
		return
	}
	assert t2.year == 2019 &&
		t2.month == 12 && t2.day == 12 && t2.hour == 6 && t2.minute == 7 && t2.second == 45
	assert t2.unix == 1576130865
}

fn test_parse_rfc2822_invalid() {
	s3 := 'Thu 12 Foo 2019 06:07:45 +0800'
	time.parse_rfc2822(s3) or {
		assert true
		return
	}
	assert false
}

fn test_parse_iso8601() {
	// Because the offset between local time and UTC is not constant in regions
	// that use daylight saving times we need to calculate separete offsets for
	// summer and winter
	offset_summer := time.Duration(time.new_time(year: 2020, month: 6, day: 5, hour: 15).local() -
		time.new_time(year: 2020, month: 6, day: 5, hour: 15))
	offset_winter := time.Duration(time.new_time(year: 2020, month: 11, day: 5, hour: 15).local() -
		time.new_time(year: 2020, month: 11, day: 5, hour: 15))
	formats := [
		'2020-06-05T15:38:06Z',
		'2020-06-05T15:38:06.015959Z',
		'2020-06-05T15:38:06.015959+00:00',
		'2020-06-05T15:38:06.015959+02:00',
		'2020-06-05T15:38:06.015959-02:00',
		'2020-11-05T15:38:06.015959Z',
	]
	times := [
		[2020, 6, 5, 15, 38, 6, 0],
		[2020, 6, 5, 15, 38, 6, 15959],
		[2020, 6, 5, 15, 38, 6, 15959],
		[2020, 6, 5, 13, 38, 6, 15959],
		[2020, 6, 5, 17, 38, 6, 15959],
		[2020, 11, 5, 15, 38, 6, 15959],
	]
	for i, format in formats {
		t := time.parse_iso8601(format) or {
			assert false
			continue
		}
		data := times[i]
		t2 := time.new_time(
			year: data[0]
			month: data[1]
			day: data[2]
			hour: data[3]
			minute: data[4]
			second: data[5]
			microsecond: data[6]
		).add(if i <= 4 { offset_summer } else { offset_winter })
		assert t.year == t2.year
		assert t.month == t2.month
		assert t.day == t2.day
		assert t.hour == t2.hour
		assert t.minute == t2.minute
		assert t.second == t2.second
		assert t.microsecond == t2.microsecond
	}
}

fn test_parse_iso8601_local() {
	format := '2020-06-05T15:38:06.015959'
	t := time.parse_iso8601(format) or {
		assert false
		return
	}
	assert t.year == 2020
	assert t.month == 6
	assert t.day == 5
	assert t.hour == 15
	assert t.minute == 38
	assert t.second == 6
	assert t.microsecond == 15959
}

fn test_parse_iso8601_invalid() {
	formats := [
		'',
		'2020-06-05X15:38:06.015959Z',
		'2020-06-05T15:38:06.015959X',
		'2020-06-05T15:38:06.015959+0000',
		'2020-06-05T',
		'2020-06-05Z',
		'2020-06-05+00:00',
		'15:38:06',
	]
	for format in formats {
		time.parse_iso8601(format) or {
			assert true
			continue
		}
		assert false
	}
}

fn test_parse_iso8601_date_only() {
	format := '2020-06-05'
	t := time.parse_iso8601(format) or {
		assert false
		return
	}
	assert t.year == 2020
	assert t.month == 6
	assert t.day == 5
	assert t.hour == 0
	assert t.minute == 0
	assert t.second == 0
	assert t.microsecond == 0
}
