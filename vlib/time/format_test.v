import time

const test_time = time.Time{
	year:       2024
	month:      7
	day:        15
	hour:       14
	minute:     30
	second:     45
	nanosecond: 123456789
}

fn test_formats() {
	assert test_time.format() == '2024-07-15 14:30'
	assert test_time.format_ss() == '2024-07-15 14:30:45'

	assert test_time.format_ss_milli() == '2024-07-15 14:30:45.123'
	assert test_time.format_ss_micro() == '2024-07-15 14:30:45.123456'
	assert test_time.format_ss_nano() == '2024-07-15 14:30:45.123456789'

	assert test_time.hhmm() == '14:30'
	assert test_time.hhmmss() == '14:30:45'
	assert test_time.hhmm12() == '2:30 p.m.'

	assert test_time.ymmdd() == '2024-07-15'
	assert test_time.ddmmy() == '15.07.2024'
	assert test_time.md() == 'Jul 15'

	assert test_time.get_fmt_date_str(.dot, .ddmmyyyy) == '15.07.2024'
	assert test_time.get_fmt_date_str(.slash, .mmddyyyy) == '07/15/2024'
	assert test_time.get_fmt_date_str(.hyphen, .yyyymmdd) == '2024-07-15'
	assert test_time.get_fmt_date_str(.space, .ddmmyyyy) == '15 07 2024'

	assert test_time.get_fmt_time_str(.hhmmss24) == '14:30:45'
	assert test_time.get_fmt_time_str(.hhmm12) == '2:30 p.m.'
	assert test_time.get_fmt_time_str(.hhmm24) == '14:30'

	assert test_time.get_fmt_str(.hyphen, .hhmm24, .yyyymmdd) == '2024-07-15 14:30'
}

fn test_format_year_boundaries() {
	// year 1970
	t1 := time.Time{
		year:   1970
		month:  1
		day:    1
		hour:   0
		minute: 0
		second: 0
	}
	assert t1.format() == '1970-01-01 00:00'

	// year 2038 (Y2K38)
	t2 := time.Time{
		year:   2038
		month:  1
		day:    19
		hour:   3
		minute: 14
		second: 7
	}
	assert t2.format_ss() == '2038-01-19 03:14:07'

	// year 9999
	t3 := time.Time{
		year:   9999
		month:  12
		day:    31
		hour:   23
		minute: 59
		second: 59
	}
	assert t3.format_ss() == '9999-12-31 23:59:59'
}

fn test_format_nanosecond_precision() {
	t1 := time.Time{
		year:       2024
		month:      1
		day:        1
		hour:       0
		minute:     0
		second:     0
		nanosecond: 1000000 // 1 millisecond
	}
	assert t1.format_ss_milli() == '2024-01-01 00:00:00.001'

	t2 := time.Time{
		year:       2024
		month:      1
		day:        1
		hour:       0
		minute:     0
		second:     0
		nanosecond: 1000 // 1 microsecond
	}
	assert t2.format_ss_micro() == '2024-01-01 00:00:00.000001'

	t3 := time.Time{
		year:       2024
		month:      1
		day:        1
		hour:       0
		minute:     0
		second:     0
		nanosecond: 1
	}
	assert t3.format_ss_nano() == '2024-01-01 00:00:00.000000001'

	// Test with max nanoseconds
	t4 := time.Time{
		year:       2024
		month:      1
		day:        1
		hour:       0
		minute:     0
		second:     0
		nanosecond: 999999999
	}
	assert t4.format_ss_nano() == '2024-01-01 00:00:00.999999999'
}

fn test_format_midnight_and_noon() {
	t1 := time.Time{
		year:   2024
		month:  6
		day:    15
		hour:   0
		minute: 0
		second: 0
	}
	assert t1.hhmm() == '00:00'
	assert t1.hhmmss() == '00:00:00'

	t2 := time.Time{
		year:   2024
		month:  6
		day:    15
		hour:   12
		minute: 0
		second: 0
	}
	assert t2.hhmm() == '12:00'
	assert t2.hhmmss() == '12:00:00'

	t3 := time.Time{
		year:   2024
		month:  6
		day:    15
		hour:   23
		minute: 59
		second: 59
	}
	assert t3.hhmm() == '23:59'
	assert t3.hhmmss() == '23:59:59'
}

fn test_format_all_months() {
	months := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
	expected := ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12']!

	for i, month in months {
		t := time.Time{
			year:   2024
			month:  month
			day:    15
			hour:   12
			minute: 0
			second: 0
		}
		result := t.format()
		assert result.contains(expected[i])
	}
}

fn test_format_leap_year() {
	t := time.Time{
		year:   2024
		month:  2
		day:    29
		hour:   12
		minute: 30
		second: 45
	}
	assert t.format_ss() == '2024-02-29 12:30:45'
}

fn test_int_to_ptr_byte_array_no_pad() {
	// Test the internal function through format methods
	// This tests that digits are correctly written without padding

	// Single digit day
	t1 := time.Time{
		year:   2024
		month:  1
		day:    1
		hour:   1
		minute: 1
		second: 1
	}
	assert t1.format() == '2024-01-01 01:01'
	assert t1.format_ss() == '2024-01-01 01:01:01'

	// All nines
	t2 := time.Time{
		year:   9999
		month:  9
		day:    9
		hour:   9
		minute: 9
		second: 9
	}
	assert t2.format() == '9999-09-09 09:09'
	assert t2.format_ss() == '9999-09-09 09:09:09'
}
