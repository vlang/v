module time

// Tests the now in all platform and the ne operator function
fn test_now_always_results_in_greater_time() {
	t1 := now()
	time.sleep_ms(1)
	t2 := now()
	assert t2.ne(t1)
}

fn test_time1_should_be_same_as_time2() {

	t1 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})

	t2 := new_time( Time {
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

	t1 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})

	// Difference is one microsecond
	t2 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 101
	})

	t3 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 0
	})

	// Difference is one second
	t4 := new_time( Time {
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

	t1 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 102
	})

	// Difference is one microsecond
	t2 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 101
	})

	t3 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 5
		microsecond: 0
	})

	// Difference is one second
	t4 := new_time( Time {
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

	t1 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 102
	})

	// Difference is one microsecond
	t2 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 101
	})

	t3 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 0
	})

	// Difference is one second
	t4 := new_time( Time {
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

	t1 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 102
	})

	// Difference is one microsecond
	t2 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 101
	})

	t3 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 5
		microsecond: 0
	})

	// Difference is one second
	t4 := new_time( Time {
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

	t1 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})

	// Difference is one microsecond
	t2 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})

	t3 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 0
	})

	// Difference is one second
	t4 := new_time( Time {
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

	t1 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})

	// Difference is one microsecond
	t2 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 101
	})

	t3 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 0
	})

	// Difference is one second
	t4 := new_time( Time {
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

	t1 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})

	// Difference is one microsecond
	t2 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	})

	t3 := new_time( Time {
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 0
	})

	// Difference is one second
	t4 := new_time( Time {
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