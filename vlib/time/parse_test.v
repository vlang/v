import time

fn test_parse() {
	s := '2018-01-27 12:48:34'
	t := time.parse(s) or {
		assert false
		return
	}
	assert t.year == 2018 && t.month == 1 && t.day == 27 && t.hour == 12 && t.minute == 48 && t.second == 34
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
	assert t1.year == 2019 && t1.month == 12 && t1.day == 12 && t1.hour == 6 && t1.minute == 7 && t1.second == 45
	assert t1.unix == 1576130865
	s2 := 'Thu 12 Dec 2019 06:07:45 +0800'
	t2 := time.parse_rfc2822(s2) or {
		assert false
		return
	}
	assert t2.year == 2019 && t2.month == 12 && t2.day == 12 && t2.hour == 6 && t2.minute == 7 && t2.second == 45
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

fn test_iso8601_parse_utc_diff() {
	format_utc 	:= '2020-06-05T15:38:06.015959+00:00'
	format_cest := '2020-06-05T15:38:06.015959+02:00'

	t_utc 	:= time.parse_iso8601(format_utc) or {panic(err)}
	t_cest 	:= time.parse_iso8601(format_cest) or {panic(err)}

	assert t_utc.year  == 2020
	assert t_cest.year == 2020
	assert t_utc.month == 6
	assert t_cest.month == 6
	assert t_utc.day == 5
	assert t_cest.day == 5
	// if it was formatted in utc it should be
	// two hours before if it was formatted in
	// cest time
	assert t_utc.hour == (t_cest.hour + 2)
	assert t_utc.minute == 38
	assert t_cest.minute == 38
	assert t_utc.second == 6
	assert t_cest.second == 6
	assert t_utc.microsecond == 15959
	assert t_cest.microsecond == 15959
}