module time

// duration_parse_limit is the largest magnitude that a parsed duration can have,
// which is the absolute value of the smallest `i64`.
const duration_parse_limit = u64(1) << 63

fn error_invalid_duration(s string) IError {
	return error('invalid duration: "${s}"')
}

// parse_duration parses a duration string, such as `'300ms'`, `'-1.5h'` or `'2h45m30.5s'`.
// A duration string is an optional sign followed by one or more decimal numbers,
// each with an optional fraction and a mandatory unit suffix.
// The valid units are `ns`, `us` (or `µs`), `ms`, `s`, `m` and `h`.
// A zero duration can also be written without a unit, as `0`, `+0` or `-0`.
// It returns an error when `s` is not a valid duration string, or when the duration
// does not fit in the `i64` range of nanoseconds that a `Duration` can hold.
// Example: assert time.parse_duration('1h30m')! == 90 * time.minute
// Example: assert time.parse_duration('-1.5s')! == -1500 * time.millisecond
// Example: assert time.parse_duration('1.000000001s')!.nanoseconds() == 1_000_000_001
pub fn parse_duration(s string) !Duration {
	mut pos := 0
	mut is_negative := false
	if s != '' && (s[0] == `-` || s[0] == `+`) {
		is_negative = s[0] == `-`
		pos = 1
	}
	if s.len == pos + 1 && s[pos] == `0` {
		return Duration(0)
	}
	if pos == s.len {
		return error_invalid_duration(s)
	}
	mut total := u64(0)
	for pos < s.len {
		if s[pos] != `.` && !s[pos].is_digit() {
			return error_invalid_duration(s)
		}
		whole, whole_end := duration_leading_int(s, pos) or { return error_invalid_duration(s) }
		mut digits := whole_end - pos
		pos = whole_end
		mut fraction := u64(0)
		mut scale := f64(1)
		if pos < s.len && s[pos] == `.` {
			fraction_start := pos + 1
			fraction, scale, pos = duration_leading_fraction(s, fraction_start)
			digits += pos - fraction_start
		}
		if digits == 0 {
			// a dot that has no digits on either side, as in `.s`
			return error_invalid_duration(s)
		}
		unit_start := pos
		for pos < s.len && s[pos] != `.` && !s[pos].is_digit() {
			pos++
		}
		if pos == unit_start {
			return error('missing unit in duration: "${s}"')
		}
		unit_name := s[unit_start..pos]
		unit := duration_unit(unit_name) or {
			return error('unknown unit "${unit_name}" in duration: "${s}"')
		}
		if whole > duration_parse_limit / unit {
			return error_invalid_duration(s)
		}
		mut value := whole * unit
		if fraction > 0 {
			// The fraction is less than one unit, so it adds at most an hour of nanoseconds.
			// An f64 resolves that to less than a nanosecond, while `fraction * unit` can
			// overflow a u64.
			value += u64(f64(fraction) * (f64(unit) / scale))
			if value > duration_parse_limit {
				return error_invalid_duration(s)
			}
		}
		if value > duration_parse_limit - total {
			return error_invalid_duration(s)
		}
		total += value
	}
	if is_negative {
		if total == duration_parse_limit {
			return Duration(min_i64)
		}
		return Duration(-i64(total))
	}
	if total > u64(max_i64) {
		return error_invalid_duration(s)
	}
	return Duration(i64(total))
}

// duration_leading_int returns the value of the decimal digits in `s` that begin at `start`,
// and the index of the first byte after them. The value is 0 when there are no digits there.
// It returns `none` when the value is larger than `duration_parse_limit`.
fn duration_leading_int(s string, start int) ?(u64, int) {
	mut value := u64(0)
	mut i := start
	for i < s.len && s[i].is_digit() {
		if value > duration_parse_limit / 10 {
			return none
		}
		value = value * 10 + u64(s[i] - `0`)
		if value > duration_parse_limit {
			return none
		}
		i++
	}
	return value, i
}

// duration_leading_fraction returns the value of the decimal digits in `s` that begin at `start`,
// the power of ten that this value has to be divided by to get the fraction that the digits
// represent, and the index of the first byte after the digits.
// The digits that no longer fit in the value are skipped instead of being an error,
// since they are below the precision of a `Duration`.
fn duration_leading_fraction(s string, start int) (u64, f64, int) {
	mut value := u64(0)
	mut scale := f64(1)
	mut is_full := false
	mut i := start
	for i < s.len && s[i].is_digit() {
		if !is_full {
			if value > (duration_parse_limit - 1) / 10 {
				is_full = true
			} else {
				next := value * 10 + u64(s[i] - `0`)
				if next > duration_parse_limit {
					is_full = true
				} else {
					value = next
					scale *= 10
				}
			}
		}
		i++
	}
	return value, scale, i
}

// duration_unit returns how many nanoseconds are in the unit `name`,
// or `none` when there is no such unit.
fn duration_unit(name string) ?u64 {
	match name {
		'ns' {
			return u64(nanosecond)
		}
		// `us` can also be written with the micro sign U+00B5,
		// or with the Greek small letter mu U+03BC, which looks the same.
		'us', '\u00b5s', '\u03bcs' {
			return u64(microsecond)
		}
		'ms' {
			return u64(millisecond)
		}
		's' {
			return u64(second)
		}
		'm' {
			return u64(minute)
		}
		'h' {
			return u64(hour)
		}
		else {
			return none
		}
	}
}
