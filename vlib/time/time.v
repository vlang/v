module time

pub const (
	days_string        = 'MonTueWedThuFriSatSun'
	month_days         = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	months_string      = 'JanFebMarAprMayJunJulAugSepOctNovDec'
	// The unsigned zero year for internal calculations.
	// Must be 1 mod 400, and times before it will not compute correctly,
	// but otherwise can be changed at will.
	absolute_zero_year = i64(-292277022399) // as i64
	seconds_per_minute = 60
	seconds_per_hour   = 60 * seconds_per_minute
	seconds_per_day    = 24 * seconds_per_hour
	seconds_per_week   = 7 * seconds_per_day
	days_per_400_years = 365 * 400 + 97
	days_per_100_years = 365 * 100 + 24
	days_per_4_years   = 365 * 4 + 1
	days_before        = [
		0,
		31,
		31 + 28,
		31 + 28 + 31,
		31 + 28 + 31 + 30,
		31 + 28 + 31 + 30 + 31,
		31 + 28 + 31 + 30 + 31 + 30,
		31 + 28 + 31 + 30 + 31 + 30 + 31,
		31 + 28 + 31 + 30 + 31 + 30 + 31 + 31,
		31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30,
		31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31,
		31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30,
		31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31,
	]
	long_days          = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday',
		'Sunday',
	]
)

// Time contains various time units for a point in time.
pub struct Time {
pub:
	year        int
	month       int
	day         int
	hour        int
	minute      int
	second      int
	microsecond int
	unix        i64
}

// smonth returns month name.
pub fn (t Time) smonth() string {
	if t.month <= 0 || t.month > 12 {
		return '---'
	}
	i := t.month - 1
	return time.months_string[i * 3..(i + 1) * 3]
}

// day_of_week returns the current day of a given year, month, and day,
// as an integer.
pub fn day_of_week(y int, m int, d int) int {
	// Sakomotho's algorithm is explained here:
	// https://stackoverflow.com/a/6385934
	t := [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
	mut sy := y
	if m < 3 {
		sy = sy - 1
	}
	return (sy + sy / 4 - sy / 100 + sy / 400 + t[m - 1] + d - 1) % 7 + 1
}

// day_of_week returns the current day as an integer.
pub fn (t Time) day_of_week() int {
	return day_of_week(t.year, t.month, t.day)
}

// weekday_str returns the current day as a string.
pub fn (t Time) weekday_str() string {
	i := t.day_of_week() - 1
	return time.days_string[i * 3..(i + 1) * 3]
}

// weekday_str returns the current day as a string.
pub fn (t Time) long_weekday_str() string {
	i := t.day_of_week() - 1
	return time.long_days[i]
}

// FormatDelimiter contains different time formats.
pub enum FormatTime {
	hhmm12
	hhmm24
	hhmmss12
	hhmmss24
	hhmmss24_milli
	hhmmss24_micro
	no_time
}

// FormatDelimiter contains different date formats.
pub enum FormatDate {
	ddmmyy
	ddmmyyyy
	mmddyy
	mmddyyyy
	mmmd
	mmmdd
	mmmddyy
	mmmddyyyy
	no_date
	yyyymmdd
	yymmdd
}

// FormatDelimiter contains different time/date delimiters.
pub enum FormatDelimiter {
	dot
	hyphen
	slash
	space
	no_delimiter
}

// A lot of these are taken from the Go library.
pub type Duration = i64

pub const (
	nanosecond  = Duration(1)
	microsecond = Duration(1000 * nanosecond)
	millisecond = Duration(1000 * microsecond)
	second      = Duration(1000 * millisecond)
	minute      = Duration(60 * second)
	hour        = Duration(60 * minute)
	infinite    = Duration(C.INT64_MAX)
)

// nanoseconds returns the duration as an integer number of nanoseconds.
pub fn (d Duration) nanoseconds() i64 {
	return i64(d)
}

// microseconds returns the duration as an integer number of microseconds.
pub fn (d Duration) microseconds() i64 {
	return i64(d) / 1000
}

// milliseconds returns the duration as an integer number of milliseconds.
pub fn (d Duration) milliseconds() i64 {
	return i64(d) / 1000000
}

// The following functions return floating point numbers because it's common to
// consider all of them in sub-one intervals
// seconds returns the duration as a floating point number of seconds.
pub fn (d Duration) seconds() f64 {
	sec := d / time.second
	nsec := d % time.second
	return f64(sec) + f64(nsec) / 1e9
}

// minutes returns the duration as a floating point number of minutes.
pub fn (d Duration) minutes() f64 {
	min := d / time.minute
	nsec := d % time.minute
	return f64(min) + f64(nsec) / (60 * 1e9)
}

// hours returns the duration as a floating point number of hours.
pub fn (d Duration) hours() f64 {
	hr := d / time.hour
	nsec := d % time.hour
	return f64(hr) + f64(nsec) / (60 * 60 * 1e9)
}
