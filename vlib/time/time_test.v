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