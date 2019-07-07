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

fn test_days_in_month() {
	assert time.days_in_month(1, 2001)?  == 31 // January
	assert time.days_in_month(2, 2001)?  == 28 // February
	assert time.days_in_month(2, 2000)?  == 29 // February (leap)
	assert time.days_in_month(3, 2001)?  == 31 // March
	assert time.days_in_month(4, 2001)?  == 30 // April
	assert time.days_in_month(5, 2001)?  == 31 // May
	assert time.days_in_month(6, 2001)?  == 30 // June
	assert time.days_in_month(7, 2001)?  == 31 // July
	assert time.days_in_month(8, 2001)?  == 31 // August
	assert time.days_in_month(9, 2001)?  == 30 // September
	assert time.days_in_month(10, 2001)? == 31 // October
	assert time.days_in_month(11, 2001)? == 30 // November
	assert time.days_in_month(12, 2001)? == 31 // December
}
