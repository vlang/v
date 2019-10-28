import time

fn test_is_leap_year() {
    assert time.is_leap_year(1700) == false
    assert time.is_leap_year(1800) == false
    assert time.is_leap_year(1900) == false

    assert time.is_leap_year(1600) == true
    assert time.is_leap_year(2000) == true

    assert time.is_leap_year(2100) == false
    assert time.is_leap_year(2200) == false
    assert time.is_leap_year(2300) == false

    assert time.is_leap_year(1996) == true
    assert time.is_leap_year(1997) == false
}

fn check(month, year, expected int) bool {
	res := time.days_in_month(month, year) or {
		return false
	}
	return res == expected 
}

fn test_days_in_month() {
	assert check(1, 2001, 31) // January
	assert check(2, 2001, 28) // February
	assert check(2, 2000, 29) // February (leap)
	assert check(3, 2001, 31) // March
	assert check(4, 2001, 30) // April
	assert check(5, 2001, 31) // May
	assert check(6, 2001, 30) // June
	assert check(7, 2001, 31) // July
	assert check(8, 2001, 31) // August
	assert check(9, 2001, 30) // September
	assert check(10, 2001, 31) // October
	assert check(11, 2001, 30) // November
	assert check(12, 2001, 31) // December
}


fn test_unix() {
	t := time.unix(1564366499) 
	assert t.year == 2019
	assert t.month == 7 
	assert t.day == 29 
	assert t.hour == 2 
	assert t.minute == 14 
	//assert t.second == 32  // TODO broken 
} 

fn test_format_ss() {
        t :=    time.Time{  year:     1980,
                            month:    7,
                            day:      11,
                            hour:     21,
                            minute:   23,
                            second:   42,
                            uni:      0 }

        assert  '11.07.1980 21:23:42' == t.get_fmt_str(time.FormatDelimiter.dot,
                                                       time.FormatTime.hhmmss24,
                                                       time.FormatDate.ddmmyyyy)
}

fn test_format() {
        t :=    time.Time{  year:     1980,
                            month:    7,
                            day:      11,
                            hour:     21,
                            minute:   23,
                            second:   42,
                            uni:      0 }

        assert  '11.07.1980 21:23' == t.get_fmt_str(time.FormatDelimiter.dot,
                                                    time.FormatTime.hhmm24,
                                                    time.FormatDate.ddmmyyyy)
}

fn test_hhmm() {
        t :=    time.Time{  year:     1980,
                            month:    7,
                            day:      11,
                            hour:     21,
                            minute:   23,
                            second:   42,
                            uni:      0 }

        assert  '21:23' == t.get_fmt_time_str(time.FormatTime.hhmm24)
}

fn test_hhmm12() {
        t :=    time.Time{  year:     1980,
                            month:    7,
                            day:      11,
                            hour:     21,
                            minute:   23,
                            second:   42,
                            uni:      0 }

        assert  '9:23 p.m.' == t.get_fmt_time_str(time.FormatTime.hhmm12)
}

fn test_hhmmss() {
        t :=    time.Time{  year:     1980,
                            month:    7,
                            day:      11,
                            hour:     21,
                            minute:   23,
                            second:   42,
                            uni:      0 }

        assert  '21:23:42' == t.get_fmt_time_str(time.FormatTime.hhmmss24)
}

fn test_ymmdd() {
        t :=    time.Time{  year:     1980,
                            month:    7,
                            day:      11,
                            hour:     21,
                            minute:   23,
                            second:   42,
                            uni:      0 }

        assert  '1980-07-11' == t.get_fmt_date_str(time.FormatDelimiter.hyphen,
                                                   time.FormatDate.yyyymmdd)
}

fn test_ddmmy() {
        t :=    time.Time{  year:     1980,
                            month:    7,
                            day:      11,
                            hour:     21,
                            minute:   23,
                            second:   42,
                            uni:      0 }

        assert  '11.07.1980' == t.get_fmt_date_str(time.FormatDelimiter.dot,
                                                   time.FormatDate.ddmmyyyy)
}

fn test_md() {
        t :=    time.Time{  year:     1980,
                            month:    7,
                            day:      11,
                            hour:     21,
                            minute:   23,
                            second:   42,
                            uni:      0 }

        assert 'Jul 11' == t.get_fmt_date_str(time.FormatDelimiter.space,
                                              time.FormatDate.mmmd)
}

fn test_get_fmt_time_str() {
        t :=    time.Time{  year:     1980,
                            month:    7,
                            day:      11,
                            hour:     21,
                            minute:   23,
                            second:   42,
                            uni:      0 }

        assert  '21:23:42' == t.get_fmt_time_str(time.FormatTime.hhmmss24)
        assert  '21:23' == t.get_fmt_time_str(time.FormatTime.hhmm24)
        assert  '9:23:42 p.m.' == t.get_fmt_time_str(time.FormatTime.hhmmss12)
        assert  '9:23 p.m.' == t.get_fmt_time_str(time.FormatTime.hhmm12)
}

fn test_get_fmt_date_str() {
        t :=    time.Time{  year:     1980,
                            month:    7,
                            day:      11,
                            hour:     21,
                            minute:   23,
                            second:   42,
                            uni:      0 }

        assert  '11.07.1980' == t.get_fmt_date_str(time.FormatDelimiter.dot,
                                                   time.FormatDate.ddmmyyyy)
        assert  '11/07/1980' == t.get_fmt_date_str(time.FormatDelimiter.slash,
                                                   time.FormatDate.ddmmyyyy)
        assert  '11-07-1980' == t.get_fmt_date_str(time.FormatDelimiter.hyphen,
                                                   time.FormatDate.ddmmyyyy)
        assert  '11 07 1980' == t.get_fmt_date_str(time.FormatDelimiter.space,
                                                   time.FormatDate.ddmmyyyy)
        assert  '07.11.1980' == t.get_fmt_date_str(time.FormatDelimiter.dot,
                                                   time.FormatDate.mmddyyyy)
        assert  '07/11/1980' == t.get_fmt_date_str(time.FormatDelimiter.slash,
                                                   time.FormatDate.mmddyyyy)
        assert  '07-11-1980' == t.get_fmt_date_str(time.FormatDelimiter.hyphen,
                                                   time.FormatDate.mmddyyyy)
        assert  '07 11 1980' == t.get_fmt_date_str(time.FormatDelimiter.space,
                                                   time.FormatDate.mmddyyyy)
        assert  '11.07.80'   == t.get_fmt_date_str(time.FormatDelimiter.dot,
                                                   time.FormatDate.ddmmyy)
        assert  '11/07/80'   == t.get_fmt_date_str(time.FormatDelimiter.slash,
                                                   time.FormatDate.ddmmyy)
        assert  '11-07-80'   == t.get_fmt_date_str(time.FormatDelimiter.hyphen,
                                                   time.FormatDate.ddmmyy)
        assert  '11 07 80'   == t.get_fmt_date_str(time.FormatDelimiter.space,
                                                   time.FormatDate.ddmmyy)
        assert  '07.11.80'   == t.get_fmt_date_str(time.FormatDelimiter.dot,
                                                   time.FormatDate.mmddyy)
        assert  '07/11/80'   == t.get_fmt_date_str(time.FormatDelimiter.slash,
                                                   time.FormatDate.mmddyy)
        assert  '07-11-80'   == t.get_fmt_date_str(time.FormatDelimiter.hyphen,
                                                   time.FormatDate.mmddyy)
        assert  '07 11 80'   == t.get_fmt_date_str(time.FormatDelimiter.space,
                                                   time.FormatDate.mmddyy)
        assert  'Jul 11'     == t.get_fmt_date_str(time.FormatDelimiter.space,
                                                   time.FormatDate.mmmd)
        assert  'Jul 11'     == t.get_fmt_date_str(time.FormatDelimiter.space,
                                                   time.FormatDate.mmmdd)
        assert  'Jul 11 1980' == t.get_fmt_date_str(time.FormatDelimiter.space,
                                                    time.FormatDate.mmmddyyyy)
        assert  '1980-07-11'  == t.get_fmt_date_str(time.FormatDelimiter.hyphen,
                                                    time.FormatDate.yyyymmdd)
}

fn test_get_fmt_str() {
        t :=    time.Time{  year:     1980,
                            month:    7,
                            day:      11,
                            hour:     21,
                            minute:   23,
                            second:   42,
                            uni:      0 }

        // Since get_fmt_time_str and get_fmt_date_str do have comprehensive
        // tests I don't want to exaggerate here with all possible
        // combinations.
        assert  '11.07.1980 21:23:42' == t.get_fmt_str(time.FormatDelimiter.dot,
                                                       time.FormatTime.hhmmss24,
                                                       time.FormatDate.ddmmyyyy)
}
