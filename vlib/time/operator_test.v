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