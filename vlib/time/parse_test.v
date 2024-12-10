import time

fn test_parse() {
	s := '2018-01-27 12:48:34'
	t := time.parse(s) or {
		assert false, '> failing format: ${s} | err: ${err}'
		return
	}
	assert t.year == 2018 && t.month == 1 && t.day == 27 && t.hour == 12 && t.minute == 48
		&& t.second == 34
	assert t.unix() == 1517057314
}

fn test_parse_invalid() {
	if x := time.parse('Invalid time string') {
		assert false
	}
	assert true

	if x := time.parse('2020-02-02 02.20.02') {
		assert false
	}
	assert true
}

fn test_parse_rfc2822() {
	s1 := 'Thu, 12 Dec 2019 06:07:45 GMT'
	t1 := time.parse_rfc2822(s1) or {
		assert false, '> failing format: ${s1} | err: ${err}'
		return
	}
	assert t1.year == 2019 && t1.month == 12 && t1.day == 12 && t1.hour == 6 && t1.minute == 7
		&& t1.second == 45
	assert t1.unix() == 1576130865
	s2 := 'Thu 12 Dec 2019 06:07:45 +0800'
	t2 := time.parse_rfc2822(s2) or {
		assert false, '> failing format: ${s2} | err: ${err}'
		return
	}
	assert t2.year == 2019 && t2.month == 12 && t2.day == 12 && t2.hour == 6 && t2.minute == 7
		&& t2.second == 45
	assert t2.unix() == 1576130865
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
		[2020, 6, 5, 15, 38, 6, 15959000],
		[2020, 6, 5, 15, 38, 6, 15959000],
		[2020, 6, 5, 13, 38, 6, 15959000],
		[2020, 6, 5, 17, 38, 6, 15959000],
		[2020, 11, 5, 15, 38, 6, 15959000],
	]
	for i, format in formats {
		t := time.parse_iso8601(format) or {
			assert false, '>>> failing format: ${format} | err: ${err}'
			continue
		}
		year := times[i][0]
		assert t.year == year
		month := times[i][1]
		assert t.month == month
		day := times[i][2]
		assert t.day == day
		hour := times[i][3]
		assert t.hour == hour
		minute := times[i][4]
		assert t.minute == minute
		second := times[i][5]
		assert t.second == second
		nanosecond := times[i][6]
		assert t.nanosecond == nanosecond
	}
}

fn test_parse_iso8601_local() {
	format := '2020-06-05T15:38:06.015959'
	t := time.parse_iso8601(format) or {
		assert false, '> failing format: ${format} | err: ${err}'
		return
	}
	assert t.year == 2020
	assert t.month == 6
	assert t.day == 5
	assert t.hour == 15
	assert t.minute == 38
	assert t.second == 6
	assert t.nanosecond == 15959_000
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
		'2020-06-32T15:38:06.015959',
		'2020-13-13T15:38:06.015959',
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
		assert false, '> failing format: ${format} | err: ${err}'
		return
	}
	assert t.year == 2020
	assert t.month == 6
	assert t.day == 5
	assert t.hour == 0
	assert t.minute == 0
	assert t.second == 0
	assert t.nanosecond == 0
}

fn check_invalid_date(s string) {
	if date := time.parse(s) {
		assert false, 'invalid date: "${s}" => "${date}"'
	}
	assert true
}

fn invalid_rfc3339(s string) string {
	if date := time.parse_rfc3339(s) {
		assert false, 'invalid date: "${s}" => "${date}"'
	} else {
		assert true
		return err.str()
	}
	return ''
}

fn test_invalid_dates_should_error_during_parse() {
	check_invalid_date('-99999-12-20 00:00:00')
	check_invalid_date('99999-12-20 00:00:00')

	check_invalid_date('2008-00-20 00:00:00')
	check_invalid_date('2008-25-20 00:00:00')

	check_invalid_date('2008-12-00 00:00:00')
	check_invalid_date('2008-12-32 00:00:00')

	check_invalid_date('2008-12-01 30:00:00')
	check_invalid_date('2008-12-01 00:60:00')
	check_invalid_date('2008-12-01 00:01:60')
}

fn test_parse_rfc3339() {
	pairs := [
		['2015-01-06T15:47:32.080254511Z', '2015-01-06 15:47:32.080254'],
		['2015-01-06T15:47:32.072697474Z', '2015-01-06 15:47:32.072697'],
		['2015-01-06T15:47:32.1234Z', '2015-01-06 15:47:32.123400'],
		['2015-01-06T15:47:32.001234Z', '2015-01-06 15:47:32.001234'],
		['2015-01-06T15:47:32Z', '2015-01-06 15:47:32.000000'],
		['2015-01-06T15:47:32+00:00', '2015-01-06 15:47:32.000000'],
		['2015-01-06T15:47:32-00:00', '2015-01-06 15:47:32.000000'],
		['2015-01-06T15:47:32-01:00', '2015-01-06 16:47:32.000000'],
		['2015-01-06T15:47:32+01:00', '2015-01-06 14:47:32.000000'],
		['2015-01-06T15:47:32-01:10', '2015-01-06 16:57:32.000000'],
		['2015-01-06T15:47:32+01:10', '2015-01-06 14:37:32.000000'],
		['2015-01-06T15:47:32.1234-00:00', '2015-01-06 15:47:32.123400'],
		['2015-01-06T15:47:32.1234+01:00', '2015-01-06 14:47:32.123400'],
		['2015-01-06T15:47:32.1234-01:00', '2015-01-06 16:47:32.123400'],
		['2015-01-06T22:59:59-00:10', '2015-01-06 23:09:59.000000'],
		['1979-05-27T07:32:00-08:00', '1979-05-27 15:32:00.000000'],
		['2024-10-19T22:47:08-00:00', '2024-10-19 22:47:08.000000'],
		['2024-10-19T22:47:08.9+00:00', '2024-10-19 22:47:08.900000'],
		['2024-10-20T01:47:08+03:00', '2024-10-19 22:47:08.000000'],
		['2024-10-20T01:47:08.981+03:00', '2024-10-19 22:47:08.981000'],
	]
	for pair in pairs {
		input, expected := pair[0], pair[1]
		res := time.parse_rfc3339(input) or {
			assert false, '>>> failing input: ${input} | err: ${err}'
			return
		}
		output := res.format_ss_micro()
		assert expected == output
	}
	assert invalid_rfc3339('22:47:08Z') == 'missing date part of RFC 3339'
	assert invalid_rfc3339('01:47:08.981+03:00') == 'missing date part of RFC 3339'
	assert invalid_rfc3339('2006-01-00') == 'date error: invalid day 0'
	assert invalid_rfc3339('2006-01-32') == 'date error: invalid day 32'
	assert invalid_rfc3339('2006-01-88') == 'date error: invalid day 88'
	assert invalid_rfc3339('2006-00-01') == 'date error: invalid month 0'
	assert invalid_rfc3339('2006-13-01') == 'date error: invalid month 13'
	assert invalid_rfc3339('2006-77-01') == 'date error: invalid month 77'
	assert invalid_rfc3339('2006-01-01T24:47:08Z') == 'invalid hour: 24'
	assert invalid_rfc3339('2006-01-01T99:47:08Z') == 'invalid hour: 99'
	assert invalid_rfc3339('2006-01-01T23:60:08Z') == 'invalid minute: 60'
	assert invalid_rfc3339('2006-01-01T23:99:08Z') == 'invalid minute: 99'
	assert invalid_rfc3339('2006-01-01T23:59:60Z') == 'invalid second: 60'
	assert invalid_rfc3339('2006-01-01T23:59:99Z') == 'invalid second: 99'
}

fn test_ad_second_to_parse_result_in_2001() {
	now_tm := time.parse('2001-01-01 04:00:00')!
	future_tm := now_tm.add_seconds(60)
	assert future_tm.str() == '2001-01-01 04:01:00'
	assert now_tm.unix() < future_tm.unix()
}

fn test_ad_second_to_parse_result_pre_2001() {
	now_tm := time.parse('2000-01-01 04:00:00')!
	future_tm := now_tm.add_seconds(60)
	assert future_tm.str() == '2000-01-01 04:01:00'
	assert now_tm.unix() < future_tm.unix()
}

fn test_parse_format() {
	mut s := '2018-01-27 12:48:34'
	mut t := time.parse_format(s, 'YYYY-MM-DD HH:mm:ss') or {
		assert false, '> failing format: ${s} | err: ${err}'
		return
	}
	assert t.year == 2018 && t.month == 1 && t.day == 27 && t.hour == 12 && t.minute == 48
		&& t.second == 34

	s = '2018-November-27 12:48:20'
	t = time.parse_format(s, 'YYYY-MMMM-DD HH:mm:ss') or {
		assert false, '> failing format: ${s} | err: ${err}'
		return
	}
	assert t.year == 2018 && t.month == 11 && t.day == 27 && t.hour == 12 && t.minute == 48
		&& t.second == 20

	s = '18-1-2 0:8:2'
	t = time.parse_format(s, 'YY-M-D H:m:s') or {
		assert false, '> failing format: ${s} | err: ${err}'
		return
	}
	assert t.year == 2018 && t.month == 1 && t.day == 2 && t.hour == 0 && t.minute == 8
		&& t.second == 2

	// This should always fail, because we test if M and D allow for a 01 value which they shouldn't
	s = '2018-01-02 1:8:2'
	t = time.parse_format(s, 'YYYY-M-D H:m:s') or { return }

	assert false, '> failing for datetime: ${s}, the datetime string should not have passed the format "YYYY-M-D H:m:s"'
}
