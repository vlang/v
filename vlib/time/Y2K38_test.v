import time

fn test_time_after_2038_works() {
	after_time := time.parse_iso8601('2037-07-23') or { time.now() }
	dump(after_time)
	error_time := after_time.add_days(180)
	dump(error_time)
	assert error_time.str() == '2038-01-19 00:00:00'
	// Note: the next date is after Y2K38, it should NOT wrap:
	error_time2 := after_time.add_days(181)
	dump(error_time2)
	assert error_time2.str() == '2038-01-20 00:00:00'
}
