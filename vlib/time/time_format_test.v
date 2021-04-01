import time

const (
	time_to_test = time.Time{
		year: 1980
		month: 7
		day: 11
		hour: 21
		minute: 23
		second: 42
		unix: 332198622
	}
)

fn test_now_format() {
	t := time.now()
	u := t.unix
	assert t.format() == time.unix(int(u)).format()
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
	assert '07-11-1980' == time_to_test.get_fmt_date_str(.hyphen, .mmddyyyy)
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
	assert 'Jul 11 80' == time_to_test.get_fmt_date_str(.space, .mmmddyy)
	assert 'Jul 11 1980' == time_to_test.get_fmt_date_str(.space, .mmmddyyyy)
	assert '1980-07-11' == time_to_test.get_fmt_date_str(.hyphen, .yyyymmdd)
	assert '80.07.11' == time_to_test.get_fmt_date_str(.dot, .yymmdd)
}

fn test_get_fmt_str() {
	// Since get_fmt_time_str and get_fmt_date_str do have comprehensive
	// tests I don't want to exaggerate here with all possible
	// combinations.
	assert '11.07.1980 21:23:42' == time_to_test.get_fmt_str(.dot, .hhmmss24, .ddmmyyyy)
}

fn test_utc_string() {
	assert 'Fri, 11 Jul 1980 21:23:42 UTC' == time_to_test.utc_string()
}
