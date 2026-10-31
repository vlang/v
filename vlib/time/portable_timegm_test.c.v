module time

fn ptg_from_fields(year int, mon int, mday int, hour int, min int, sec int) i64 {
	mut tm := C.tm{}
	tm.tm_year = year - 1900
	tm.tm_mon = mon
	tm.tm_mday = mday
	tm.tm_hour = hour
	tm.tm_min = min
	tm.tm_sec = sec
	return portable_timegm(&tm)
}

fn calendar_unix(year int, mon int, mday int, hour int, min int, sec int) i64 {
	return new(year: year, month: mon, day: mday, hour: hour, minute: min, second: sec).unix()
}

// A zeroed `C.tm` is 1900-01-00, i.e. the day before 1900-01-01, because
// `tm_mday` is passed straight through without the normalisation `Time.new`
// applies.
fn test_portable_timegm_of_a_zeroed_tm_is_the_day_before_the_zero_year() {
	assert portable_timegm(unsafe { &C.tm{} }) == -2209075200
}

fn test_portable_timegm_at_the_unix_epoch() {
	assert ptg_from_fields(1970, 0, 1, 0, 0, 0) == 0
	assert ptg_from_fields(1970, 0, 1, 0, 0, 1) == 1
	assert ptg_from_fields(1970, 0, 2, 0, 0, 0) == 86400
	assert ptg_from_fields(1970, 1, 1, 0, 0, 0) == 2678400
	assert ptg_from_fields(1970, 11, 31, 23, 59, 59) == 31535999
}

fn test_portable_timegm_goes_negative_before_the_epoch() {
	assert ptg_from_fields(1969, 11, 31, 0, 0, 0) == -86400
	assert ptg_from_fields(1969, 11, 31, 23, 59, 59) == -1
	assert ptg_from_fields(1900, 0, 1, 0, 0, 0) == -2208988800
	assert ptg_from_fields(1, 0, 1, 0, 0, 0) == -62135596800
}

fn test_portable_timegm_at_the_32_bit_boundary() {
	// The largest value a signed 32-bit second count can hold.
	assert ptg_from_fields(2038, 0, 19, 3, 14, 7) == 2147483647
	assert ptg_from_fields(2038, 0, 19, 3, 14, 8) == 2147483648
	assert ptg_from_fields(2038, 0, 19, 3, 14, 7) != ptg_from_fields(2038, 0, 19, 3, 14, 8)
}

fn test_portable_timegm_knows_leap_years() {
	assert ptg_from_fields(2000, 1, 28, 0, 0, 0) == 951782400 - 86400
	assert ptg_from_fields(2000, 1, 29, 0, 0, 0) == 951782400
	assert ptg_from_fields(2000, 2, 1, 0, 0, 0) == 951868800
	assert ptg_from_fields(2001, 2, 1, 0, 0, 0) == 983404800
	// 2100 is divisible by 100 but not by 400, so it is not a leap year.
	assert ptg_from_fields(2100, 2, 1, 0, 0, 0) - ptg_from_fields(2100, 1, 28, 0, 0, 0) == 86400
	// 2000 is divisible by 400, so it is a leap year.
	assert ptg_from_fields(2000, 2, 1, 0, 0, 0) - ptg_from_fields(2000, 1, 28, 0, 0, 0) == 2 * 86400
}

fn test_portable_timegm_known_timestamps() {
	assert ptg_from_fields(2004, 1, 29, 12, 34, 56) == 1078058096
	assert ptg_from_fields(2000, 8, 29, 0, 0, 0) == 970185600
	assert ptg_from_fields(2000, 9, 1, 0, 0, 0) == 970358400
	assert ptg_from_fields(2026, 5, 15, 13, 45, 21) == 1781531121
	assert ptg_from_fields(2025, 10, 1, 0, 0, 0) == 1761955200
}

// 2006-03-12 was a daylight-saving changeover day in most of the northern
// hemisphere. portable_timegm is calendar arithmetic, so it must not care.
fn test_portable_timegm_ignores_local_daylight_saving() {
	assert ptg_from_fields(2006, 2, 12, 6, 0, 0) == 1142143200
	assert ptg_from_fields(2006, 2, 12, 7, 0, 0) == 1142146800
	assert ptg_from_fields(2006, 2, 12, 8, 0, 0) == 1142150400
	assert ptg_from_fields(2006, 2, 12, 8, 0, 0) - ptg_from_fields(2006, 2, 12, 6, 0, 0) == 7200
}

// `tm_mon` is 0-based, but an out-of-range value is normalised the same way
// the C comment above the function describes, instead of being rejected. A
// month past 11 rolls into January of the next year; a negative month rolls
// back into December of the previous one.
fn test_portable_timegm_normalises_out_of_range_months() {
	assert ptg_from_fields(2026, 11, 1, 0, 0, 0) == 1796083200
	assert ptg_from_fields(2026, 12, 1, 0, 0, 0) == ptg_from_fields(2027, 0, 1, 0, 0, 0)
	assert ptg_from_fields(2026, 13, 1, 0, 0, 0) == ptg_from_fields(2027, 1, 1, 0, 0, 0)
	// tm_mon -1 is December of the previous year, not November.
	assert ptg_from_fields(2026, -1, 1, 0, 0, 0) == 1764547200
	assert ptg_from_fields(2026, -1, 1, 0, 0, 0) == ptg_from_fields(2025, 11, 1, 0, 0, 0)
	assert ptg_from_fields(2026, -11, 1, 0, 0, 0) == ptg_from_fields(2025, 1, 1, 0, 0, 0)
	assert ptg_from_fields(2026, 23, 1, 0, 0, 0) == ptg_from_fields(2027, 11, 1, 0, 0, 0)
}

// portable_timegm and `Time.new` compute the same calendar independently, so
// their agreement is evidence the golden values above are not arbitrary.
fn test_portable_timegm_agrees_with_time_new() {
	for year in [1, 100, 1600, 1900, 1969, 1970, 1971, 2000, 2004, 2024, 2026, 2038, 2100] {
		for mon in [1, 2, 3, 7, 12] {
			for mday in [1, 15, 28] {
				a := ptg_from_fields(year, mon - 1, mday, 13, 45, 21)
				b := calendar_unix(year, mon, mday, 13, 45, 21)
				assert a == b, 'year ${year}, month ${mon}, day ${mday}: ${a} != ${b}'
			}
		}
	}
}

fn test_portable_timegm_counts_the_days_in_a_year() {
	for year in [2023, 2100] {
		start := ptg_from_fields(year, 0, 1, 0, 0, 0)
		end := ptg_from_fields(year + 1, 0, 1, 0, 0, 0)
		assert end - start == 365 * 86400, 'year ${year} was ${end - start} seconds long'
	}
	for year in [2000, 2024] {
		start := ptg_from_fields(year, 0, 1, 0, 0, 0)
		end := ptg_from_fields(year + 1, 0, 1, 0, 0, 0)
		assert end - start == 366 * 86400, 'leap year ${year} was ${end - start} seconds long'
	}
}
