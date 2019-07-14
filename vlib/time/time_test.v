import time

fn test_is_leap_year() {
    assert time.is_leap_year(1700) == false
    assert time.is_leap_year(1800) == false
    assert time.is_leap_year(1900) == false

    assert time.is_leap_year(1600) == true
    assert time.is_leap_year(2000) == true

    assert time.is_leap_year(2100) == false
    assert time.is_leap_year(2200) == false
    assert time.is_leap_year(2300) == false

    assert time.is_leap_year(1996) == true
    assert time.is_leap_year(1997) == false
}

fn check(month, year, expected int) bool {
	res := time.days_in_month(month, year) or {
		return false
	}
	return res == expected 
}

fn test_days_in_month() {
	assert check(1, 2001, 31) // January
	assert check(2, 2001, 28) // February
	assert check(2, 2000, 29) // February (leap)
	assert check(3, 2001, 31) // March
	assert check(4, 2001, 30) // April
	assert check(5, 2001, 31) // May
	assert check(6, 2001, 30) // June
	assert check(7, 2001, 31) // July
	assert check(8, 2001, 31) // August
	assert check(9, 2001, 30) // September
	assert check(10, 2001, 31) // October
	assert check(11, 2001, 30) // November
	assert check(12, 2001, 31) // December
}
