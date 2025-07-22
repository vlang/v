module time

// A lot of these are taken from the Go library.
pub type Duration = i64

pub const nanosecond = Duration(1)
pub const microsecond = Duration(1000 * nanosecond)
pub const millisecond = Duration(1000 * microsecond)
pub const second = Duration(1000 * millisecond)
pub const minute = Duration(60 * second)
pub const hour = Duration(60 * minute)
//	day         = Duration(24 * hour)
pub const infinite = Duration(i64(9223372036854775807))

// nanoseconds returns the duration as an integer number of nanoseconds.
pub fn (d Duration) nanoseconds() i64 {
	return i64(d)
}

// microseconds returns the duration as an integer number of microseconds.
pub fn (d Duration) microseconds() i64 {
	return i64(d) / microsecond
}

// milliseconds returns the duration as an integer number of milliseconds.
pub fn (d Duration) milliseconds() i64 {
	return i64(d) / millisecond
}

// The following functions return floating point numbers because it's common to
// consider all of them in sub-one intervals
// seconds returns the duration as a floating point number of seconds.
pub fn (d Duration) seconds() f64 {
	return f64(d) / f64(second)
}

// minutes returns the duration as a floating point number of minutes.
pub fn (d Duration) minutes() f64 {
	return f64(d) / f64(minute)
}

// hours returns the duration as a floating point number of hours.
pub fn (d Duration) hours() f64 {
	return f64(d) / f64(hour)
}

// days returns the duration as a floating point number of days.
pub fn (d Duration) days() f64 {
	return f64(d) / f64(hour * 24)
}

// str pretty prints the duration
//
// ```
// h:m:s      // 5:02:33
// m:s.mi<s>  // 2:33.015
// s.mi<s>    // 33.015s
// mi.mc<ms>  // 15.007ms
// mc.ns<ns>  // 7.234us
// ns<ns>     // 234ns
// ```
pub fn (d Duration) str() string {
	if d == infinite {
		return 'inf'
	}
	mut sign := ''
	mut t := i64(d)
	if t < 0 {
		sign = '-'
		t = -t
	}
	hr := t / hour
	t -= hr * hour
	min := t / minute
	t -= min * minute
	sec := t / second
	t -= sec * second
	ms := t / millisecond
	t -= ms * millisecond
	us := t / microsecond
	t -= us * microsecond
	ns := t

	return match true {
		hr > 0 { '${sign}${hr}:${min:02}:${sec:02}' }
		min > 0 { '${sign}${min}:${sec:02}.${ms:03}' }
		sec > 0 { '${sign}${sec}.${ms:03}s' }
		ms > 0 { '${sign}${ms}.${us:03}ms' }
		us > 0 { '${sign}${us}.${ns:03}us' }
		else { '${sign}${ns}ns' }
	}
}

// debug returns a detailed breakdown of the Duration, as: 'Duration: - 50days, 4h, 3m, 7s, 541ms, 78us, 9ns'.
pub fn (d Duration) debug() string {
	mut res := []string{}
	mut x := i64(d)
	mut sign := ''
	if x < 0 {
		sign = '- '
		x = -x
	}
	for label, v in {
		'days': 24 * hour
		'h':    hour
		'm':    minute
		's':    second
		'ms':   millisecond
		'us':   microsecond
	} {
		if x > v {
			xx := x / v
			x = x % v
			res << xx.str() + label
		}
	}
	if x > 0 {
		res << '${x}ns'
	}
	return 'Duration: ${sign}${res.join(', ')}'
}
