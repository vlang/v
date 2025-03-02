import time
import math

const local_time_to_test = time.new(
	year:       1980
	month:      7
	day:        11
	hour:       21
	minute:     23
	second:     42
	nanosecond: 123456789
	is_local:   true
)

const utc_time_to_test = time.new(
	year:       1980
	month:      7
	day:        11
	hour:       21
	minute:     23
	second:     42
	nanosecond: 123456789
	is_local:   false
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

fn check_days_in_month(month int, year int, expected int) bool {
	res := time.days_in_month(month, year) or { return false }
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
	t2 := time.unix(1078058096)
	assert t2.year == 2004
	assert t2.month == 2
	assert t2.day == 29
	assert t2.hour == 12
	assert t2.minute == 34
	assert t2.second == 56
	t3 := time.unix(1070236799)
	assert t3.year == 2003
	assert t3.month == 11
	assert t3.day == 30
	assert t3.hour == 23
	assert t3.minute == 59
	assert t3.second == 59
	t4 := time.unix(1577783439)
	assert t4.year == 2019
	assert t4.month == 12
	assert t4.day == 31
	assert t4.hour == 9
	assert t4.minute == 10
	assert t4.second == 39
	t5 := time.unix(-1824922433)
	assert t5.year == 1912
	assert t5.month == 3
	assert t5.day == 4
	assert t5.hour == 5
	assert t5.minute == 6
	assert t5.second == 7
	t6 := time.unix(1577858969)
	assert t6.year == 2020
	assert t6.month == 1
	assert t6.day == 1
	assert t6.hour == 6
	assert t6.minute == 9
	assert t6.second == 29
	assert local_time_to_test.unix() == 332198622
	assert utc_time_to_test.unix() == 332198622
}

fn test_format_rfc3339() {
	// assert '1980-07-11T19:23:42.123Z'
	res := local_time_to_test.format_rfc3339()
	assert res.ends_with('23:42.123Z')
	assert res.starts_with('1980-07-1')
	assert res.contains('T')

	// assert '1980-07-11T19:23:42.123Z'
	utc_res := utc_time_to_test.format_rfc3339()
	assert utc_res.ends_with('23:42.123Z')
	assert utc_res.starts_with('1980-07-1')
	assert utc_res.contains('T')
}

fn test_format_rfc3339_micro() {
	res := local_time_to_test.format_rfc3339_micro()
	assert res.ends_with('23:42.123456Z')
	assert res.starts_with('1980-07-1')
	assert res.contains('T')

	utc_res := utc_time_to_test.format_rfc3339_micro()
	assert utc_res.ends_with('23:42.123456Z')
	assert utc_res.starts_with('1980-07-1')
	assert utc_res.contains('T')
}

fn test_format_rfc3339_nano() {
	res := local_time_to_test.format_rfc3339_nano()
	assert res.ends_with('23:42.123456789Z')
	assert res.starts_with('1980-07-1')
	assert res.contains('T')

	utc_res := utc_time_to_test.format_rfc3339_nano()
	assert utc_res.ends_with('23:42.123456789Z')
	assert utc_res.starts_with('1980-07-1')
	assert utc_res.contains('T')
}

fn test_format_ss() {
	assert '11.07.1980 21:23:42' == local_time_to_test.get_fmt_str(.dot, .hhmmss24, .ddmmyyyy)

	assert '11.07.1980 21:23:42' == utc_time_to_test.get_fmt_str(.dot, .hhmmss24, .ddmmyyyy)
}

fn test_format_ss_milli() {
	assert '11.07.1980 21:23:42.123' == local_time_to_test.get_fmt_str(.dot, .hhmmss24_milli,
		.ddmmyyyy)
	assert '1980-07-11 21:23:42.123' == local_time_to_test.format_ss_milli()

	assert '11.07.1980 21:23:42.123' == utc_time_to_test.get_fmt_str(.dot, .hhmmss24_milli,
		.ddmmyyyy)
	assert '1980-07-11 21:23:42.123' == utc_time_to_test.format_ss_milli()
}

fn test_format_ss_micro() {
	assert '11.07.1980 21:23:42.123456' == local_time_to_test.get_fmt_str(.dot, .hhmmss24_micro,
		.ddmmyyyy)
	assert '1980-07-11 21:23:42.123456' == local_time_to_test.format_ss_micro()

	assert '11.07.1980 21:23:42.123456' == utc_time_to_test.get_fmt_str(.dot, .hhmmss24_micro,
		.ddmmyyyy)
	assert '1980-07-11 21:23:42.123456' == utc_time_to_test.format_ss_micro()
}

fn test_format_ss_nano() {
	assert '11.07.1980 21:23:42.123456789' == local_time_to_test.get_fmt_str(.dot, .hhmmss24_nano,
		.ddmmyyyy)
	assert '1980-07-11 21:23:42.123456789' == local_time_to_test.format_ss_nano()

	assert '11.07.1980 21:23:42.123456789' == utc_time_to_test.get_fmt_str(.dot, .hhmmss24_nano,
		.ddmmyyyy)
	assert '1980-07-11 21:23:42.123456789' == utc_time_to_test.format_ss_nano()
}

fn test_smonth() {
	month_names := ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov',
		'Dec']
	for i, name in month_names {
		month_num := i + 1
		t := time.Time{
			year:   1980
			month:  month_num
			day:    1
			hour:   0
			minute: 0
			second: 0
		}
		assert t.smonth() == name
	}
}

fn test_day_of_week() {
	for i in 0 .. 7 {
		day_of_week := i + 1
		// 2 Dec 2019 is Monday
		t := time.Time{
			year:   2019
			month:  12
			day:    2 + i
			hour:   0
			minute: 0
			second: 0
		}
		assert day_of_week == t.day_of_week()
	}
}

fn test_week_of_year() {
	// As windows use msvcrt.dll, which `strftime` does not support %V, so skip test
	// TODO: newer version windows use ucrtbase.dll, which support %V
	$if !windows {
		for year in 2000 .. 2100 {
			mut t := time.new(time.Time{
				year:  year
				month: 12
				day:   20
			})

			// check from year.12.20 to next_year.1.8
			for _ in 0 .. 20 {
				assert t.strftime('%V') == '${t.week_of_year():02}', '${t}'
				t = t.add_days(1)
			}
		}
	}

	t1 := time.Time{
		year:  2025
		month: 3
		day:   3
	}
	assert t1.week_of_year() == 10
	assert t1.add_days(1).week_of_year() == 10
}

fn test_year_day() {
	// testing if December 31st in a leap year is numbered as 366
	assert time.parse('2024-12-31 20:00:00')!.year_day() == 366

	// testing December 31st's number in a non leap year
	assert time.parse('2025-12-31 20:00:00')!.year_day() == 365

	assert time.parse('2024-02-28 20:00:00')!.year_day() == 59
	assert time.parse('2024-02-29 20:00:00')!.year_day() == 60
	assert time.parse('2024-03-01 20:00:00')!.year_day() == 61
	assert time.parse('2024-03-02 20:00:00')!.year_day() == 62

	assert time.parse('2025-02-28 20:00:00')!.year_day() == 59
	assert time.parse('2025-03-01 20:00:00')!.year_day() == 60

	assert time.parse('2024-01-01 20:00:00')!.year_day() == 1
	assert time.parse('2025-01-01 20:00:00')!.year_day() == 1
}

fn test_weekday_str() {
	day_names := ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']
	for i, name in day_names {
		// 2 Dec 2019 is Monday
		t := time.Time{
			year:   2019
			month:  12
			day:    2 + i
			hour:   0
			minute: 0
			second: 0
		}
		assert t.weekday_str() == name
	}
}

fn test_add() {
	d_seconds := 3
	d_nanoseconds := 13
	duration := time.Duration(d_seconds * time.second + d_nanoseconds * time.nanosecond)
	// dump(duration.debug())
	t1 := local_time_to_test
	// dump(t1.debug())
	t2 := local_time_to_test.add(duration)
	// dump(t2.debug())
	assert t2.second == t1.second + d_seconds
	assert t2.nanosecond == t1.nanosecond + d_nanoseconds
	assert t2.unix() == t1.unix() + d_seconds
	assert t2.is_local == t1.is_local

	t3 := local_time_to_test.add(-duration)
	// dump(t3.debug())
	assert t3.second == t1.second - d_seconds
	assert t3.nanosecond == t1.nanosecond - d_nanoseconds
	assert t3.unix() == t1.unix() - d_seconds
	assert t3.is_local == t1.is_local

	t4 := local_time_to_test.as_local()
	// dump(t4.debug())
	t5 := t4.add(duration)
	// dump(t5.debug())
	assert t5.is_local == t4.is_local

	t := time.Time{
		year:  2024
		month: 4
		day:   3
	}
	t_5am := t.add(time.hour * 5)
	assert t_5am.hour == 5
	next_day := t_5am.add_days(1)
	assert next_day.day == 4 && next_day.day == t_5am.day + 1
	assert next_day.year == t_5am.year && next_day.month == t.month
	assert next_day.month == t_5am.month && next_day.month == t.month
	assert next_day.hour == t_5am.hour && next_day.month == t.month
}

fn test_add_days() {
	num_of_days := 3
	t := local_time_to_test.add_days(num_of_days)
	assert t.day == local_time_to_test.day + num_of_days
	assert t.unix() == local_time_to_test.unix() + 86400 * num_of_days
}

fn test_str() {
	assert '1980-07-11 21:23:42' == local_time_to_test.str()

	assert '1980-07-11 21:23:42' == utc_time_to_test.str()
}

// not optimal test but will find obvious bugs
fn test_now() {
	now := time.now()
	// The year the test was built
	assert now.year >= 2020
	assert now.month > 0
	assert now.month <= 12
	assert now.minute >= 0
	assert now.minute < 60
	assert now.second >= 0
	assert now.second <= 60 // <= 60 cause of leap seconds
	assert now.nanosecond >= 0
	assert now.nanosecond < time.second
}

fn test_utc() {
	now := time.utc()
	// The year the test was built
	// dump(now.debug())
	assert now.year >= 2020
	assert now.month > 0
	assert now.month <= 12
	assert now.minute >= 0
	assert now.minute < 60
	assert now.second >= 0
	assert now.second <= 60 // <= 60 cause of leap seconds
	assert now.nanosecond >= 0
	assert now.nanosecond < time.second
}

fn test_unix_time() {
	t1 := time.utc()
	time.sleep(50 * time.millisecond)
	t2 := time.utc()
	eprintln('  t1: ${t1}')
	eprintln('  t2: ${t2}')
	ut1 := t1.unix()
	ut2 := t2.unix()
	eprintln(' ut1: ${ut1}')
	eprintln(' ut2: ${ut2}')
	assert ut2 - ut1 < 2

	utm1 := t1.unix_milli()
	utm2 := t2.unix_milli()
	eprintln('utm1: ${utm1}')
	eprintln('utm2: ${utm2}')
	assert (utm1 - ut1 * 1000) < 1000
	assert (utm2 - ut2 * 1000) < 1000

	assert utm2 - utm1 > 2
	assert utm2 - utm1 < 999
}

fn test_offset() {
	u := time.utc()
	n := time.now()

	mut diff_seconds := 0
	if u.day != n.day {
		if u.day > n.day {
			diff_seconds = int(math.abs(((u.hour * 60 + u.minute) - (n.hour * 60 + n.minute)) * 60)) - 86400
		} else {
			diff_seconds = 86400 - int(math.abs(((u.hour * 60 + u.minute) - (n.hour * 60 + n.minute)) * 60))
		}
		if math.abs(u.day - n.day) > 1 { // different month
			diff_seconds = diff_seconds * -1
		}
	} else { // same day
		diff_seconds = ((n.hour * 60 + n.minute) - (u.hour * 60 + u.minute)) * 60
	}

	assert diff_seconds == time.offset()
}

fn test_since() {
	t1 := time.now()
	time.sleep(20 * time.millisecond)
	d1 := time.since(t1)
	assert d1 >= 20_000_000
	time.sleep(20 * time.millisecond)
	d2 := time.since(t1)
	assert d2 >= 40_000_000
}

// issue relate https://github.com/vlang/v/issues/13828
// problem: the local method add 2h on the time in a Linux machine
// the other machine are not tested in a local env
fn test_recursive_local_call() {
	now_tm := time.now()
	assert now_tm.str() == now_tm.local().str()
	assert now_tm.local().str() == now_tm.local().local().str()
}

fn test_strftime() {
	assert '1980 July 11' == local_time_to_test.strftime('%Y %B %d')

	assert '1980 July 11' == utc_time_to_test.strftime('%Y %B %d')
}

fn test_add_seconds_to_time() {
	now_tm := time.now()
	future_tm := now_tm.add_seconds(60)
	assert now_tm.unix() < future_tm.unix()
}

fn test_plus_equals_duration() {
	mut d := time.second
	d += time.second
	assert d == 2 * time.second
}

fn test_parse_three_letters_month() {
	tm := time.now()
	format := 'MMM DD HH:mm:ss YYYY'
	tm_s := tm.custom_format(format)
	tm_tm := time.parse_format(tm_s, format)!
	assert tm_tm.month == tm.month
}

fn test_parse_ordinal_weekday_d() {
	format := 'd MMM DD HH:mm:ss YYYY'
	dt := '0 Jan 01 00:00:00 1970'
	tm := time.parse_format(dt, format)!
	tm_s := tm.custom_format(format)
	assert tm_s == '4 Jan 01 00:00:00 1970'
}

fn test_parse_ordinal_weekday_c() {
	format := 'c MMM DD HH:mm:ss YYYY'
	dt := '7 Jan 01 00:00:00 1970'
	tm := time.parse_format(dt, format)!
	tm_s := tm.custom_format(format)
	assert tm_s == '4 Jan 01 00:00:00 1970'
}

fn test_parse_two_letters_weekday() {
	format := 'dd MMM DD HH:mm:ss YYYY'
	dt := 'Su Jan 01 00:00:00 1970'
	tm := time.parse_format(dt, format)!
	tm_s := tm.custom_format(format)
	assert tm_s == 'Th Jan 01 00:00:00 1970'
}

fn test_parse_three_letters_weekday() {
	format := 'ddd MMM DD HH:mm:ss YYYY'
	dt := 'Sun Jan 01 00:00:00 1970'
	tm := time.parse_format(dt, format)!
	tm_s := tm.custom_format(format)
	assert tm_s == 'Thu Jan 01 00:00:00 1970'
}

fn test_parse_weekday() {
	format := 'dddd MMM DD HH:mm:ss YYYY'
	dt := 'Sunday Jan 01 00:00:00 1970'
	tm := time.parse_format(dt, format)!
	tm_s := tm.custom_format(format)
	assert tm_s == 'Thursday Jan 01 00:00:00 1970'
}
