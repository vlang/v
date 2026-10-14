module time

fn test_weekday_known_dates() {
	d := new(Time{
		year:  2026
		month: 10
		day:   14
	}) // a Wednesday
	assert d.weekday() == .wednesday
	e := new(Time{
		year:  2000
		month: 1
		day:   1
	}) // a Saturday
	assert e.weekday() == .saturday
	f := new(Time{
		year:  1970
		month: 1
		day:   1
	}) // a Thursday
	assert f.weekday() == .thursday
	sun := new(Time{
		year:  2024
		month: 12
		day:   1
	})
	assert sun.weekday() == .sunday
}

fn test_add_date_months() {
	t := new(Time{
		year:  2026
		month: 1
		day:   31
	})
	feb := t.add_date(0, 1, 0)
	assert feb.year == 2026 && feb.month == 2 && feb.day == 28, 'Jan 31 + 1M = Feb 28'
	mar := t.add_date(0, 2, 0)
	assert mar.month == 3 && mar.day == 31, 'Jan 31 + 2M = Mar 31'
	year_back := t.add_date(0, -1, 0)
	assert year_back.year == 2025 && year_back.month == 12 && year_back.day == 31
	multi_year := t.add_date(2, 13, 0)
	assert multi_year.year == 2029 && multi_year.month == 2, '2y13m normalizes to Feb'
}

fn test_add_date_days_and_time_preserved() {
	t := new(Time{
		year:   2026
		month:  12
		day:    31
		hour:   13
		minute: 45
		second: 30
	})
	next := t.add_date(0, 0, 1)
	assert next.year == 2027 && next.month == 1 && next.day == 1
	assert next.hour == 13 && next.minute == 45 && next.second == 30, 'time of day must be preserved'
	back := t.add_date(0, 0, -31)
	assert back.year == 2026 && back.month == 11 && back.day == 30
	leap := new(Time{
		year:  2024
		month: 2
		day:   28
	})
	assert leap.add_date(0, 0, 1).day == 29, '2024 is a leap year'
	assert leap.add_date(1, 0, 0).day == 28, '2025 is not'
	assert new(Time{
		year:  2000
		month: 2
		day:   28
	}).add_date(0, 0, 1).day == 29, '2000 is a leap year'
	not_leap := new(Time{
		year:  1900
		month: 2
		day:   28
	})
	rolled := not_leap.add_date(0, 0, 1)
	assert rolled.month == 3 && rolled.day == 1, '1900 is not a leap year: rolls to March'
}

fn test_clock_is_monotonic() {
	first := clock()
	assert first >= 0
	mut spins := 0
	for spins < 5_000_000 {
		spins++
	}
	after := clock()
	assert after >= first, 'clock must never go backwards'
}
