module time

// Weekday is the day of the week in ISO 8601 numbering (Monday = 1 …
// Sunday = 7), matching the builtin `day_of_week` and `Time.day_of_week`.
pub enum Weekday {
	monday = 1
	tuesday
	wednesday
	thursday
	friday
	saturday
	sunday = 7
}

// weekday returns the day of the week of t.
pub fn (t Time) weekday() Weekday {
	d := day_of_week(t.year, t.month, t.day)
	return unsafe { Weekday(d) }
}

// add_date returns t shifted by the given number of years, months and days,
// normalizing months and clamping the day to the target month's last day.
// The time of day, nanoseconds and location are preserved. Ambiguous wall
// times prefer the input's UTC offset when valid; nonexistent wall times
// use that offset, normalizing to a nearby valid time.
pub fn (t Time) add_date(years int, months int, days int) Time {
	if years == 0 && months == 0 && days == 0 {
		return t
	}
	mut year := t.year + years + (t.month - 1 + months) / 12
	mut month := (t.month - 1 + months) % 12
	if month < 0 {
		month += 12
		year--
	}
	month++
	// days_in_month takes (month, year)
	dim := days_in_month(month, year) or { 0 }
	day := if t.day > dim { dim } else { t.day }
	total_days := i64(days_from_civil(year, month, day)) + i64(days)
	ny, nm, nd := civil_from_days(total_days)
	calendar := new(Time{
		year:       ny
		month:      nm
		day:        nd
		hour:       t.hour
		minute:     t.minute
		second:     t.second
		nanosecond: t.nanosecond
		is_local:   t.is_local
	})
	if loc := t.location() {
		wall := calendar.local_unix()
		input_offset := loc.offset_at(t.unix()) or { panic(err.msg()) }
		mut instant := wall - i64(input_offset)
		target_offset := loc.offset_at(instant) or { panic(err.msg()) }
		if target_offset != input_offset {
			adjusted := wall - i64(target_offset)
			// Keep the input offset in a gap, where neither candidate matches.
			if (loc.offset_at(adjusted) or { panic(err.msg()) }) == target_offset {
				instant = adjusted
			}
		}
		return loc.unix_nanosecond_to_local(instant, t.nanosecond) or { panic(err.msg()) }
	}
	return calendar
}

// clock returns a monotonic clock reading in milliseconds.
// Its origin is platform-dependent; subtract readings to measure elapsed time.
pub fn clock() i64 {
	return i64(sys_mono_now() / 1_000_000)
}

// days_from_civil returns days since 1970-01-01 for a 1-based (year, month,
// day) triple (Howard Hinnant's algorithm). It is the inverse of
// civil_from_days and deliberately independent of the March-based
// days_from_unix_epoch, so callers cannot mix conventions.
fn days_from_civil(y_in int, m int, d int) int {
	mut y := y_in
	if m <= 2 {
		y--
	}
	era := (if y >= 0 { y } else { y - 399 }) / 400
	yoe := y - era * 400 // [0, 399]
	doy := (153 * (m + (if m > 2 { -3 } else { 9 })) + 2) / 5 + d - 1 // [0, 365]
	doe := yoe * 365 + yoe / 4 - yoe / 100 + doy // [0, 146096]
	return era * 146097 + doe - 719468
}

// civil_from_days converts days since 1970-01-01 into a (year, month, day)
// triple (Howard Hinnant's algorithm).
fn civil_from_days(z0 i64) (int, int, int) {
	mut z := z0 + 719468
	era := (if z >= 0 { z } else { z - 146096 }) / 146097
	doe := z - era * 146097 // [0, 146096]
	yoe := (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365 // [0, 399]
	y := yoe + era * 400
	doy := doe - (365 * yoe + yoe / 4 - yoe / 100) // [0, 365]
	mp := (5 * doy + 2) / 153 // [0, 11]
	d := doy - (153 * mp + 2) / 5 + 1 // [1, 31]
	m := if mp < 10 { mp + 3 } else { mp - 9 } // [1, 12]
	mut yy := y
	if m <= 2 {
		yy++
	}
	return yy, m, d
}
