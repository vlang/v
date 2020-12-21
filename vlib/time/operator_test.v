module time

fn assert_greater_time(ms int, t1 Time) {
	sleep_ms(ms)
	t2 := now()
	assert t2.gt(t1)
}

// Tests the now in all platform and the gt operator function with at least ms resolution
fn test_now_always_results_in_greater_time() {
	t1 := now()
	$if macos {
		assert_greater_time(1, t1)
		return
	}
	$if windows {
		// Lower resolution of time for windows
		assert_greater_time(15, t1)
		return
	}
	$if linux {
		assert_greater_time(1, t1)
		return
	}
	$if solaris {
		assert_greater_time(1, t1)
		return
	}
	// other platforms may have more accurate resolution,
	// but we do not know that ... so wait at least 1s:
	assert_greater_time(1001, t1)
}

fn test_time1_should_be_same_as_time2() {
	t1 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})
	t2 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})
	assert t1.eq(t2)
}

fn test_time1_should_not_be_same_as_time2() {
	t1 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})
	// Difference is one microsecond
	t2 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 101
	})
	t3 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 0
	})
	// Difference is one second
	t4 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 4
		microsecond: 0
	})
	assert t1.ne(t2)
	assert t3.ne(t4)
}

fn test_time1_should_be_greater_than_time2() {
	t1 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 102
	})
	// Difference is one microsecond
	t2 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 101
	})
	t3 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 5
		microsecond: 0
	})
	// Difference is one second
	t4 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 4
		microsecond: 0
	})
	assert t1.gt(t2)
	assert t3.gt(t4)
}

fn test_time2_should_be_less_than_time1() {
	t1 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 102
	})
	// Difference is one microsecond
	t2 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 101
	})
	t3 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 0
	})
	// Difference is one second
	t4 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 2
		microsecond: 0
	})
	assert t2.lt(t1)
	assert t4.lt(t3)
}

fn test_time1_should_be_greater_or_equal_to_time2_when_gt() {
	t1 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 102
	})
	// Difference is one microsecond
	t2 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 101
	})
	t3 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 5
		microsecond: 0
	})
	// Difference is one second
	t4 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 4
		microsecond: 0
	})
	assert t1.ge(t2)
	assert t3.ge(t4)
}

fn test_time1_should_be_greater_or_equal_to_time2_when_eq() {
	t1 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})
	// Difference is one microsecond
	t2 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})
	t3 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 0
	})
	// Difference is one second
	t4 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 0
	})
	assert t1.ge(t2)
	assert t3.ge(t4)
}

fn test_time1_should_be_less_or_equal_to_time2_when_lt() {
	t1 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})
	// Difference is one microsecond
	t2 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 101
	})
	t3 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 0
	})
	// Difference is one second
	t4 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 4
		microsecond: 0
	})
	assert t1.le(t2)
	assert t3.le(t4)
}

fn test_time1_should_be_less_or_equal_to_time2_when_eq() {
	t1 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})
	// Difference is one microsecond
	t2 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})
	t3 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 0
	})
	// Difference is one second
	t4 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 0
	})
	assert t1.le(t2)
	assert t3.le(t4)
}

fn test_time2_copied_from_time1_should_be_equal() {
	t1 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})
	t2 := new_time(t1)
	assert t2.eq(t1)
}

fn test_subtract() {
	d_seconds := 3
	d_microseconds := 13
	duration := d_seconds * second + d_microseconds * microsecond
	t1 := new_time(Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})
	t2 := unix2(int(t1.unix) + d_seconds, t1.microsecond + d_microseconds)
	d1 := t2 - t1
	d2 := t1 - t2
	assert d1 > 0
	assert d1 == duration
	assert d2 < 0
	assert d2 == -duration
}
