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

fn test_now_format_ymd_hm() {
	t := time.now()
	u := t.unix
	assert t.format_ymd_hm() == time.unix(u).format_ymd_hm()
}

fn test_format_ymd_hm() {
	assert '11.07.1980 21:23' == time_to_test.format(.dot, .hhmm24, .ddmmyyyy)
}

fn test_format_hm() {
	assert '21:23' == time_to_test.format_hm()
}

fn test_format_hm12() {
	assert '9:23 p.m.' == time_to_test.format_hm12()
}

fn test_format_hms() {
	assert '21:23:42' == time_to_test.format_hms()
}

fn test_format_ymd() {
	assert '1980-07-11' == time_to_test.format_ymd()
}

fn test_format_dmy() {
	assert '11.07.1980' == time_to_test.format_dmy()
}

fn test_format_md() {
	assert 'Jul 11' == time_to_test.format_md()
}

fn test_format_time() {
	assert '21:23:42' == time_to_test.format_time(.hhmmss24)
	assert '21:23' == time_to_test.format_time(.hhmm24)
	assert '9:23:42 p.m.' == time_to_test.format_time(.hhmmss12)
	assert '9:23 p.m.' == time_to_test.format_time(.hhmm12)
}

fn test_format_date() {
	assert '11.07.1980' == time_to_test.format_date(.dot, .ddmmyyyy)
	assert '11/07/1980' == time_to_test.format_date(.slash, .ddmmyyyy)
	assert '11-07-1980' == time_to_test.format_date(.hyphen, .ddmmyyyy)
	assert '11 07 1980' == time_to_test.format_date(.space, .ddmmyyyy)
	assert '07.11.1980' == time_to_test.format_date(.dot, .mmddyyyy)
	assert '07/11/1980' == time_to_test.format_date(.slash, .mmddyyyy)
	assert '07-11-1980' == time_to_test.format_date(.hyphen, .mmddyyyy)
	assert '07 11 1980' == time_to_test.format_date(.space, .mmddyyyy)
	assert '11.07.80' == time_to_test.format_date(.dot, .ddmmyy)
	assert '11/07/80' == time_to_test.format_date(.slash, .ddmmyy)
	assert '11-07-80' == time_to_test.format_date(.hyphen, .ddmmyy)
	assert '11 07 80' == time_to_test.format_date(.space, .ddmmyy)
	assert '07.11.80' == time_to_test.format_date(.dot, .mmddyy)
	assert '07/11/80' == time_to_test.format_date(.slash, .mmddyy)
	assert '07-11-80' == time_to_test.format_date(.hyphen, .mmddyy)
	assert '07 11 80' == time_to_test.format_date(.space, .mmddyy)
	assert 'Jul 11' == time_to_test.format_date(.space, .mmmd)
	assert 'Jul 11' == time_to_test.format_date(.space, .mmmdd)
	assert 'Jul 11 1980' == time_to_test.format_date(.space, .mmmddyyyy)
	assert '1980-07-11' == time_to_test.format_date(.hyphen, .yyyymmdd)
}

fn test_format() {
	// Since get_fmt_time_str and get_fmt_date_str do have comprehensive
	// tests I don't want to exaggerate here with all possible
	// combinations.
	assert '11.07.1980 21:23:42' == time_to_test.format(.dot, .hhmmss24, .ddmmyyyy)
}
