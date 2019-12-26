import time

const (
	time_to_test = time.new_time(time.Time {
		year: 1980, month:	7, day: 11,
		hour: 21, minute: 23, second: 42
	})
)

fn test_is_leap_year() {
	// 1996 % 4 = 0 and 1996 % 100 > 0
	assert time.is_leap_year(1996) == true

	// 2000 % 4 = 0 and 2000 % 400 = 0
	assert time.is_leap_year(2000) == true

	// 1996 % 4 > 0
	assert time.is_leap_year(1997) == false

	// 2000 % 4 = 0 and 2000 % 100 = 0
	assert time.is_leap_year(2100) == false
}

fn check_days_in_month(month, year, expected int) bool {
	res := time.days_in_month(month, year) or {
		return false
	}
	return res == expected
}

fn test_days_in_month() {
	days_in_month := [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

	for i, days in days_in_month {
		month := i + 1

		assert check_days_in_month(month, 2001, days)
	}
}

fn test_unix() {
	t := time.unix(1564366499)
	assert t.year == 2019
	assert t.month == 7
	assert t.day == 29
	assert t.hour == 2
	assert t.minute == 14
	assert t.second == 59
}

fn test_format_ss() {
	assert '11.07.1980 21:23:42' == time_to_test.get_fmt_str(.dot, .hhmmss24, .ddmmyyyy)
}

fn test_smonth() {
	month_names := [
		'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
		'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
	]

	for i, name in month_names {
		month_num := i + 1

		t := time.Time {
			year: 1980, month: month_num, day: 1,
			hour: 0, minute: 0, second: 0, uni: 0
		}

		assert t.smonth() == name
	}
}

fn test_format() {
	assert '11.07.1980 21:23' == time_to_test.get_fmt_str(.dot, .hhmm24, .ddmmyyyy)
}

fn test_hhmm() {
	assert '21:23' == time_to_test.hhmm()
}

fn test_hhmm12() {
	assert '9:23 p.m.' == time_to_test.hhmm12()
}

fn test_hhmmss() {
	assert '21:23:42' == time_to_test.hhmmss()
}

fn test_ymmdd() {
	assert '1980-07-11' == time_to_test.ymmdd()
}

fn test_ddmmy() {
	assert '11.07.1980' == time_to_test.ddmmy()
}

fn test_md() {
	assert 'Jul 11' == time_to_test.md()
}

fn test_day_of_week() {
	for i := 0; i < 7; i++ {
		day_of_week := i + 1

		// 2 Dec 2019 is Monday
		t := time.Time {
			year: 2019, month: 12, day: 2 + i,
			hour: 0, minute: 0, second: 0, uni: 0
		}

		assert day_of_week == t.day_of_week()
	}
}

fn test_weekday_str() {
	day_names := ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']

	for i, name in day_names {
		// 2 Dec 2019 is Monday
		t := time.Time {
			year: 2019, month: 12, day: 2 + i,
			hour: 0, minute: 0, second: 0, uni: 0
		}

		assert t.weekday_str() == name
	}
}

fn test_add_days() {
	num_of_days := 3
	t := time_to_test.add_days(num_of_days)

	assert t.day == time_to_test.day + num_of_days
}

fn test_get_fmt_time_str() {
	assert '21:23:42' == time_to_test.get_fmt_time_str(.hhmmss24)
	assert '21:23' == time_to_test.get_fmt_time_str(.hhmm24)
	assert '9:23:42 p.m.' == time_to_test.get_fmt_time_str(.hhmmss12)
	assert '9:23 p.m.' == time_to_test.get_fmt_time_str(.hhmm12)
}

fn test_get_fmt_date_str() {
	assert '11.07.1980' == time_to_test.get_fmt_date_str(.dot, .ddmmyyyy)
	assert '11/07/1980' == time_to_test.get_fmt_date_str(.slash, .ddmmyyyy)
	assert '11-07-1980' == time_to_test.get_fmt_date_str(.hyphen, .ddmmyyyy)
	assert '11 07 1980' == time_to_test.get_fmt_date_str(.space, .ddmmyyyy)

	assert '07.11.1980' == time_to_test.get_fmt_date_str(.dot, .mmddyyyy)
	assert '07/11/1980' == time_to_test.get_fmt_date_str(.slash, .mmddyyyy)
	assert '07-11-1980' == time_to_test.get_fmt_date_str(.hyphen,	.mmddyyyy)
	assert '07 11 1980' == time_to_test.get_fmt_date_str(.space, .mmddyyyy)

	assert '11.07.80' == time_to_test.get_fmt_date_str(.dot, .ddmmyy)
	assert '11/07/80' == time_to_test.get_fmt_date_str(.slash, .ddmmyy)
	assert '11-07-80' == time_to_test.get_fmt_date_str(.hyphen, .ddmmyy)
	assert '11 07 80' == time_to_test.get_fmt_date_str(.space, .ddmmyy)

	assert '07.11.80' == time_to_test.get_fmt_date_str(.dot, .mmddyy)
	assert '07/11/80' == time_to_test.get_fmt_date_str(.slash, .mmddyy)
	assert '07-11-80' == time_to_test.get_fmt_date_str(.hyphen, .mmddyy)
	assert '07 11 80' == time_to_test.get_fmt_date_str(.space, .mmddyy)

	assert 'Jul 11' == time_to_test.get_fmt_date_str(.space, .mmmd)
	assert 'Jul 11' == time_to_test.get_fmt_date_str(.space, .mmmdd)

	assert 'Jul 11 1980' == time_to_test.get_fmt_date_str(.space, .mmmddyyyy)

	assert '1980-07-11' == time_to_test.get_fmt_date_str(.hyphen, .yyyymmdd)
}

fn test_get_fmt_str() {
	// Since get_fmt_time_str and get_fmt_date_str do have comprehensive
	// tests I don't want to exaggerate here with all possible
	// combinations.
	assert '11.07.1980 21:23:42' == time_to_test.get_fmt_str(
		.dot, .hhmmss24, .ddmmyyyy
	)
}

fn test_parse() {
	s := '2018-01-27 12:48:34'
	t := time.parse(s)
	assert t.year == 2018 && t.month == 1 && t.day == 27
	    && t.hour == 12 && t.minute == 48 && t.second == 34
}

fn test_parse_iso() {
	s1 := 'Thu, 12 Dec 2019 06:07:45 GMT'
	t1 := time.parse_iso(s1)
	assert t1.year == 2019 && t1.month == 12 && t1.day == 12
	    && t1.hour == 6 && t1.minute == 7 && t1.second == 45

	s2 := 'Thu 12 Dec 2019 06:07:45 +0800'
	t2 := time.parse_iso(s2)
	assert t2.year == 2019 && t2.month == 12 && t2.day == 12
	    && t2.hour == 6 && t2.minute == 7 && t2.second == 45

	s3 := 'Thu 12 Foo 2019 06:07:45 +0800'
	t3 := time.parse_iso(s3)
	assert t3.year == 0 && t3.month == 0 && t3.day == 0
	    && t3.hour == 0 && t3.minute == 0 && t3.second == 0
}
