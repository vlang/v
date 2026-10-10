import time

// The dates before the year 1 are in the proleptic Gregorian calendar, where the year 0 is
// 1 BC, and a leap year. A 400 year cycle of that calendar (146097 days) starts on March 1st
// of a year that is a multiple of 400.
// The expected dates in this file come from a walk back from 1970-01-01, one day at a time,
// with the leap year rule, and not from the other conversions of the time module.

// cycle_boundary_days has [days after 1970-01-01, year, month, day], from the latest day
// to the earliest one.
const cycle_boundary_days = [
	[-719162, 1, 1, 1],
	[-719163, 0, 12, 31],
	[-719467, 0, 3, 2],
	[-719468, 0, 3, 1],
	[-719469, 0, 2, 29],
	[-719470, 0, 2, 28],
	[-719528, 0, 1, 1],
	[-719529, -1, 12, 31],
	[-865563, -400, 3, 3],
	[-865564, -400, 3, 2],
	[-865565, -400, 3, 1],
	[-865566, -400, 2, 29],
	[-865567, -400, 2, 28],
	[-1011660, -800, 3, 3],
	[-1011661, -800, 3, 2],
	[-1011662, -800, 3, 1],
	[-1011663, -800, 2, 29],
	[-1011664, -800, 2, 28],
	[-1157757, -1200, 3, 3],
	[-1157758, -1200, 3, 2],
	[-1157759, -1200, 3, 1],
	[-1157760, -1200, 2, 29],
	[-1157761, -1200, 2, 28],
]

// is_leap is the Gregorian leap year rule, which holds for the years below 1 too.
fn is_leap(year int) bool {
	return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)
}

// CalendarDay is a date, and its number of days after 1970-01-01.
struct CalendarDay {
mut:
	number int
	year   int = 1970
	month  int = 1
	day    int = 1
}

// step_back moves `c` to the previous day.
fn (mut c CalendarDay) step_back() {
	c.number--
	c.day--
	if c.day > 0 {
		return
	}
	c.month--
	if c.month == 0 {
		c.month = 12
		c.year--
	}
	c.day = match c.month {
		2 {
			if is_leap(c.year) { 29 } else { 28 }
		}
		4, 6, 9, 11 {
			30
		}
		else {
			31
		}
	}
}

fn test_date_from_days_after_unix_epoch_at_the_400_year_cycle_boundaries() {
	for p in cycle_boundary_days {
		t := time.date_from_days_after_unix_epoch(p[0])
		assert [t.year, t.month, t.day] == p[1..], 'day ${p[0]}'
	}
}

fn test_unix_at_the_400_year_cycle_boundaries() {
	for p in cycle_boundary_days {
		midnight := i64(p[0]) * time.seconds_per_day
		first := time.unix(midnight)
		assert [first.year, first.month, first.day] == p[1..], 'unix ${midnight}'
		assert [first.hour, first.minute, first.second] == [0, 0, 0], 'unix ${midnight}'
		assert first.unix() == midnight
		last := time.unix(midnight + 86399)
		assert [last.year, last.month, last.day] == p[1..], 'unix ${midnight + 86399}'
		assert [last.hour, last.minute, last.second] == [23, 59, 59], 'unix ${midnight + 86399}'
		assert last.unix() == midnight + 86399
		// the same instant, in milliseconds and in microseconds
		milli := time.unix_milli(midnight * 1_000)
		assert [milli.year, milli.month, milli.day] == p[1..], 'unix_milli of ${midnight} s'
		micro := time.unix_micro(midnight * 1_000_000)
		assert [micro.year, micro.month, micro.day] == p[1..], 'unix_micro of ${midnight} s'
	}
}

fn test_date_from_days_after_unix_epoch_for_every_day_back_to_the_year_minus_1300() {
	mut c := CalendarDay{}
	mut boundaries := 0
	for c.year >= -1300 {
		t := time.date_from_days_after_unix_epoch(c.number)
		assert t.year == c.year && t.month == c.month && t.day == c.day, 'day ${c.number} is ${c.year}-${c.month}-${c.day}, got ${t.year}-${t.month}-${t.day}'
		// the walk itself has to pass through the fixed points above
		if boundaries < cycle_boundary_days.len && cycle_boundary_days[boundaries][0] == c.number {
			assert [c.year, c.month, c.day] == cycle_boundary_days[boundaries][1..]
			boundaries++
		}
		c.step_back()
	}
	assert boundaries == cycle_boundary_days.len
	// 1970-01-01 is 1194344 days after -1301-12-31
	assert c.number == -1194344
	assert [c.year, c.month, c.day] == [-1301, 12, 31]
}

fn test_unix_round_trips_for_days_back_to_the_year_minus_1300() {
	mut c := CalendarDay{}
	for c.year >= -1300 {
		// every 37th day, and the first days of March, each at another second of the day
		if c.number % 37 == 0 || (c.month == 3 && c.day <= 3) {
			second_of_day := i64(-c.number) * 7919 % time.seconds_per_day
			seconds := i64(c.number) * time.seconds_per_day + second_of_day
			t := time.unix(seconds)
			assert t.year == c.year && t.month == c.month && t.day == c.day, 'unix ${seconds} is on ${c.year}-${c.month}-${c.day}, got ${t.year}-${t.month}-${t.day}'
			assert i64(t.hour) * 3600 + t.minute * 60 + t.second == second_of_day, 'unix ${seconds}'
			assert t.unix() == seconds
		}
		c.step_back()
	}
}

fn test_unix_far_from_1970() {
	// [unix time / 86400, year, month, day], around the starts of the 400 year cycles that are
	// 20000 cycles away from the year 0; 20000 * 146097 days do not fit 32 bits.
	far_days := [
		[i64(-2922659469), -8000000, 2, 29],
		[i64(-2922659468), -8000000, 3, 1],
		[i64(-2922659467), -8000000, 3, 2],
		[i64(2921220531), 8000000, 2, 29],
		[i64(2921220532), 8000000, 3, 1],
		[i64(2921220533), 8000000, 3, 2],
	]
	for p in far_days {
		t := time.unix(p[0] * time.seconds_per_day + 86399)
		assert [i64(t.year), t.month, t.day] == p[1..], 'day ${p[0]}'
	}
}
