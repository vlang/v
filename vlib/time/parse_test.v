import time
import math

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
	// Because there is a small difference between time.now() - time.utc and actual offset,
	// round to the nearest hour.
	offset := time.Duration(i64(math.round((time.now() - time.utc()).hours())) * time.hour)
	formats := [
		'2020-06-05T15:38:06Z',
		'2020-06-05T15:38:06.015959Z',
		'2020-06-05T15:38:06.015959+00:00',
		'2020-06-05T15:38:06.015959+02:00',
		'2020-06-05T15:38:06.015959-02:00',
	]
	times := [
		[2020, 6, 5, 15, 38, 6, 0],
		[2020, 6, 5, 15, 38, 6, 15959],
		[2020, 6, 5, 15, 38, 6, 15959],
		[2020, 6, 5, 13, 38, 6, 15959],
		[2020, 6, 5, 17, 38, 6, 15959],
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
		).add(offset)
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
	format_utc := '2020-06-05T15:38:06.015959'
	t_utc := time.parse_iso8601(format_utc) or {
		assert false
		return
	}
	assert t_utc.year == 2020
	assert t_utc.month == 6
	assert t_utc.day == 5
}
