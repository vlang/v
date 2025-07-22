module time

pub const days_string = 'MonTueWedThuFriSatSun'
pub const long_days = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']!
pub const month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]!
pub const months_string = 'JanFebMarAprMayJunJulAugSepOctNovDec'
pub const long_months = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August',
	'September', 'October', 'November', 'December']
// The unsigned zero year for internal calculations.
// Must be 1 mod 400, and times before it will not compute correctly,
// but otherwise can be changed at will.
pub const absolute_zero_year = i64(-292277022399)
pub const seconds_per_minute = 60
pub const seconds_per_hour = 60 * seconds_per_minute
pub const seconds_per_day = 24 * seconds_per_hour
pub const seconds_per_week = 7 * seconds_per_day
pub const days_per_400_years = days_in_year * 400 + 97
pub const days_per_100_years = days_in_year * 100 + 24
pub const days_per_4_years = days_in_year * 4 + 1
pub const days_in_year = 365
pub const days_before = [
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
]!

// Time contains various time units for a point in time.
pub struct Time {
	unix i64
pub:
	year       int
	month      int
	day        int
	hour       int
	minute     int
	second     int
	nanosecond int
	is_local   bool // used to make time.now().local().local() == time.now().local()
}

// FormatDelimiter contains different time formats.
pub enum FormatTime {
	hhmm12
	hhmm24
	hhmmss12
	hhmmss24
	hhmmss24_milli
	hhmmss24_micro
	hhmmss24_nano
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

// Time.new static method returns a time struct with the calculated Unix time.
pub fn Time.new(t Time) Time {
	return time_with_unix(t)
}

// new returns a time struct with the calculated Unix time.
pub fn new(t Time) Time {
	return time_with_unix(t)
}

// smonth returns the month name abbreviation.
pub fn (t Time) smonth() string {
	if t.month <= 0 || t.month > 12 {
		return '---'
	}
	i := t.month - 1
	return months_string[i * 3..(i + 1) * 3]
}

// unix returns the UNIX time with second resolution.
@[inline]
pub fn (t Time) unix() i64 {
	return time_with_unix(t).unix
}

// unix_milli returns the UNIX time with millisecond resolution.
@[inline]
pub fn (t Time) unix_milli() i64 {
	return t.unix() * 1_000 + (i64(t.nanosecond) / 1_000_000)
}

// unix_micro returns the UNIX time with microsecond resolution.
@[inline]
pub fn (t Time) unix_micro() i64 {
	return t.unix() * 1_000_000 + (i64(t.nanosecond) / 1_000)
}

// unix_nano returns the UNIX time with nanosecond resolution.
@[inline]
pub fn (t Time) unix_nano() i64 {
	// TODO: use i128 here, when V supports it, since the following expression overflows for years like 3001:
	return t.unix() * 1_000_000_000 + i64(t.nanosecond)
}

// add returns a new time with the given duration added.
pub fn (t Time) add(duration_in_nanosecond Duration) Time {
	// This expression overflows i64 for big years (and we do not have i128 yet):
	// nanos := t.unix * 1_000_000_000 + i64(t.nanosecond) <-
	// ... so instead, handle the addition manually in parts ¯\_(ツ)_/¯
	mut increased_time_nanosecond := i64(t.nanosecond) + duration_in_nanosecond.nanoseconds()
	// increased_time_second
	mut increased_time_second := t.unix() + (increased_time_nanosecond / second)
	increased_time_nanosecond = increased_time_nanosecond % second
	if increased_time_nanosecond < 0 {
		increased_time_second--
		increased_time_nanosecond += second
	}
	res := unix_nanosecond(increased_time_second, int(increased_time_nanosecond))
	return if t.is_local { res.as_local() } else { res }
}

// add_seconds returns a new time struct with an added number of seconds.
pub fn (t Time) add_seconds(seconds int) Time {
	return time_with_unix(t).add(seconds * second)
}

// add_days returns a new time struct with an added number of days.
pub fn (t Time) add_days(days int) Time {
	return time_with_unix(t).add(days * 24 * hour)
}

// since returns the time duration elapsed since a given time.
pub fn since(t Time) Duration {
	return now() - t
}

// relative returns a string representation of the difference between t
// and the current time.
//
// Sample outputs:
// ```
// // future
// now
// in 5 minutes
// in 1 day
// on Feb 17
// // past
// 2 hours ago
// last Jan 15
// 5 years ago
// ```
pub fn (t Time) relative() string {
	znow := now()
	mut secs := znow.unix - t.unix()
	mut prefix := ''
	mut suffix := ''
	if secs < 0 {
		secs *= -1
		prefix = 'in '
	} else {
		suffix = ' ago'
	}
	if secs < seconds_per_minute / 2 {
		return 'now'
	}
	if secs < seconds_per_hour {
		m := secs / seconds_per_minute
		if m == 1 {
			return '${prefix}1 minute${suffix}'
		}
		return '${prefix}${m} minutes${suffix}'
	}
	if secs < seconds_per_hour * 24 {
		h := secs / seconds_per_hour
		if h == 1 {
			return '${prefix}1 hour${suffix}'
		}
		return '${prefix}${h} hours${suffix}'
	}
	if secs < seconds_per_hour * 24 * 7 {
		d := secs / seconds_per_hour / 24
		if d == 1 {
			return '${prefix}1 day${suffix}'
		}
		return '${prefix}${d} days${suffix}'
	}
	if secs < seconds_per_hour * 24 * days_in_year {
		if prefix == 'in ' {
			return 'on ${t.md()}'
		}
		return 'last ${t.md()}'
	}
	y := secs / seconds_per_hour / 24 / days_in_year
	if y == 1 {
		return '${prefix}1 year${suffix}'
	}
	return '${prefix}${y} years${suffix}'
}

// relative_short returns a string saying how long ago a time occurred as follows:
// 0-30 seconds: `"now"`; 30-60 seconds: `"1m"`; anything else is rounded to the
// nearest minute, hour, day, or year
//
// Sample outputs:
// ```
// // future
// now
// in 5m
// in 1d
// // past
// 2h ago
// 5y ago
// ```
pub fn (t Time) relative_short() string {
	znow := now()
	mut secs := znow.unix - t.unix()
	mut prefix := ''
	mut suffix := ''
	if secs < 0 {
		secs *= -1
		prefix = 'in '
	} else {
		suffix = ' ago'
	}
	if secs < seconds_per_minute / 2 {
		return 'now'
	}
	if secs < seconds_per_hour {
		m := secs / seconds_per_minute
		if m == 1 {
			return '${prefix}1m${suffix}'
		}
		return '${prefix}${m}m${suffix}'
	}
	if secs < seconds_per_hour * 24 {
		h := secs / seconds_per_hour
		if h == 1 {
			return '${prefix}1h${suffix}'
		}
		return '${prefix}${h}h${suffix}'
	}
	if secs < seconds_per_hour * 24 * days_in_year {
		d := secs / seconds_per_hour / 24
		if d == 1 {
			return '${prefix}1d${suffix}'
		}
		return '${prefix}${d}d${suffix}'
	}
	y := secs / seconds_per_hour / 24 / days_in_year
	if y == 1 {
		return '${prefix}1y${suffix}'
	}
	return '${prefix}${y}y${suffix}'
}

// day_of_week returns the current day of a given year, month, and day, as an integer.
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

// week_of_year returns the current week of year as an integer.
// follow ISO 8601 standard
pub fn (t Time) week_of_year() int {
	// ISO 8601 Week of Year Rules:
	// --------------------------------------------
	// 1. Week Definition:
	//    - A week starts on ​**Monday**​ (Day 1) and ends on ​**Sunday**​ (Day 7).
	// 2. First Week of the Year:
	//    - The first week is the one containing the year's ​**first Thursday**.
	//    - Equivalently, the week with January 4th always belongs to Week 1.
	// 3. Year Assignment:
	//    - Dates in December/January may belong to the previous/next ISO year,
	//      depending on the week's Thursday.
	// 4. Week Number Format:
	//    - Expressed as `YYYY-Www` (e.g., `2026-W01` for the first week of 2026).
	// --------------------------------------------
	// Algorithm Steps:
	// 1. Find the Thursday of the current week:
	//    - If date is Monday-Wednesday, add days to reach Thursday.
	//    - If date is Thursday-Sunday, subtract days to reach Thursday.
	// 2. The ISO year is the calendar year of this Thursday.
	// 3. Compute the week number as:
	//    week_number = (thursday's day_of_year - 1) / 7 + 1
	day_of_week := t.day_of_week()
	days_to_thursday := 4 - day_of_week
	thursday_date := t.add_days(days_to_thursday)
	thursday_day_of_year := thursday_date.year_day()
	week_number := (thursday_day_of_year - 1) / 7 + 1
	return week_number
}

// year_day returns the current day of the year as an integer.
// See also #Time.custom_format .
pub fn (t Time) year_day() int {
	yday := t.day + days_before[t.month - 1]
	if is_leap_year(t.year) && t.month > 2 {
		return yday + 1
	}
	return yday
}

// weekday_str returns the current day as a string 3 letter abbreviation.
pub fn (t Time) weekday_str() string {
	i := t.day_of_week() - 1
	return long_days[i][0..3]
}

// long_weekday_str returns the current day as a string.
pub fn (t Time) long_weekday_str() string {
	i := t.day_of_week() - 1
	return long_days[i]
}

// is_leap_year checks if a given a year is a leap year.
pub fn is_leap_year(year int) bool {
	return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
}

// days_in_month returns a number of days in a given month.
pub fn days_in_month(month int, year int) !int {
	if month > 12 || month < 1 {
		return error('Invalid month: ${month}')
	}
	extra := if month == 2 && is_leap_year(year) { 1 } else { 0 }
	res := month_days[month - 1] + extra
	return res
}

// debug returns detailed breakdown of time (`Time{ year: YYYY month: MM day: dd hour: HH: minute: mm second: ss nanosecond: nanos unix: unix }`).
pub fn (t Time) debug() string {
	return 'Time{ year: ${t.year:04} month: ${t.month:02} day: ${t.day:02} hour: ${t.hour:02} minute: ${t.minute:02} second: ${t.second:02} nanosecond: ${t.nanosecond:09} unix: ${t.unix:07} }'
}

// offset returns time zone UTC offset in seconds.
pub fn offset() int {
	t := utc()
	local := t.local()
	return int(local.unix - t.unix)
}

// local_to_utc converts the receiver `t` to the corresponding UTC time, if it contains local time.
// If the receiver already does contain UTC time, it returns it unchanged.
pub fn (t Time) local_to_utc() Time {
	if !t.is_local {
		return t
	}
	return Time{
		...t.add(-offset() * second)
		is_local: false
	}
}

// utc_to_local converts the receiver `u` to the corresponding local time, if it contains UTC time.
// If the receiver already does contain local time, it returns it unchanged.
pub fn (u Time) utc_to_local() Time {
	if u.is_local {
		return u
	}
	return Time{
		...u.add(offset() * second)
		is_local: true
	}
}

// as_local returns the exact same time, as the receiver `t`, but with its .is_local field set to true.
// See also #Time.utc_to_local .
pub fn (t Time) as_local() Time {
	return Time{
		...t
		is_local: true
	}
}

// as_utc returns the exact same time, as the receiver `t`, but with its .is_local field set to false.
// See also #Time.local_to_utc .
pub fn (t Time) as_utc() Time {
	return Time{
		...t
		is_local: false
	}
}

// is_utc returns true, when the receiver `t` is a UTC time, and false otherwise.
// See also #Time.utc_to_local .
pub fn (t Time) is_utc() bool {
	return !t.is_local
}
