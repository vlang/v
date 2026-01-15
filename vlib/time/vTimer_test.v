module time

// Test basic timer creation
fn test_new_timer() {
	timer := new_timer()
	assert timer.freq > 0, 'Frequency should be initialized'
	assert timer.start_t == 0, 'Start time should be zero'
	assert timer.end_t == 0, 'End time should be zero'
	assert timer.running == false, 'Timer should not be running'
}

// Test timer start
fn test_timer_start() {
	mut timer := new_timer()
	timer.start()

	assert timer.running == true, 'Timer should be running after start'
	assert timer.start_t > 0, 'Start time should be set'
	assert timer.end_t == 0, 'End time should be zero while running'
}

// Test timer stop
fn test_timer_stop() {
	mut timer := new_timer()
	timer.start()
	sleep(millisecond.times(10))
	timer.stop()

	assert timer.running == false, 'Timer should not be running after stop'
	assert timer.start_t > 0, 'Start time should still be set'
	assert timer.end_t > 0, 'End time should be set'
	assert timer.end_t > timer.start_t, 'End time should be after start time'
}

// Test elapsed time measurement
fn test_timer_elapsed() {
	mut timer := new_timer()
	timer.start()

	// Sleep for approximately 50ms
	sleep(millisecond.times(50))
	timer.stop()

	elapsed_ns := timer.ns()
	elapsed_ms := timer.ns_to_ms()

	// Check that elapsed time is reasonable (between 40ms and 100ms)
	assert elapsed_ns > 40_000_000, 'Should have elapsed at least 40ms (${elapsed_ns} ns)'
	assert elapsed_ns < 200_000_000, 'Should have elapsed less than 200ms (${elapsed_ns} ns)'

	assert elapsed_ms >= 40.0, 'Milliseconds should be at least 40 (${elapsed_ms})'
	assert elapsed_ms < 150.0, 'Milliseconds should be less than 150 (${elapsed_ms})'
}

// Test microseconds conversion
fn test_timer_microseconds() {
	mut timer := new_timer()
	timer.start()
	sleep(millisecond.times(10))
	timer.stop()

	us := timer.ns_to_us()
	assert us > 9000, 'Should be at least 9000 microseconds'
	assert us < 60000, 'Should be less than 60000 microseconds'
}

// Test seconds conversion
fn test_timer_seconds() {
	mut timer := new_timer()
	timer.start()
	sleep(millisecond.times(100))
	timer.stop()

	secs := timer.ns_to_secs()
	assert secs >= 0.09, 'Should be at least 0.09 seconds (${secs})'
	assert secs < 0.2, 'Should be less than 0.2 seconds (${secs})'
}

// test_timer_minutes Test minutes conversion
fn test_timer_minutes() {
	mut timer := new_timer()
	timer.start()
	sleep(millisecond.times(10))
	timer.stop()

	mins := timer.ns_to_mins()
	assert mins > 0.0, 'Minutes should be greater than 0'
	assert mins < 0.01, 'Should be less than 0.01 minutes (${mins})'
}

// Test hours conversion
fn test_timer_hours() {
	mut timer := new_timer()
	timer.start()
	sleep(millisecond.times(100))
	timer.stop()

	hrs := timer.ns_to_hrs()
	assert hrs > 0.0, 'Hours should be greater than 0'
	assert hrs < 0.001, 'Should be very small hours (${hrs})'
}

// Test days conversion
fn test_timer_days() {
	mut timer := new_timer()
	timer.start()
	sleep(millisecond.times(100))
	timer.stop()

	days := timer.ns_to_days()
	assert days > 0.0, 'Days should be greater than 0'
	assert days < 0.00001, 'Should be very small days (${days})'
}

// Test now_ns function
fn test_now_ns() {
	ns1 := now_ns()
	sleep(millisecond.times(10))
	ns2 := now_ns()

	assert ns2 > ns1, 'Second reading should be greater than first'
	diff := ns2 - ns1
	assert diff > 9_000_000, 'Difference should be at least 9ms (${diff} ns)'
	assert diff < 41_000_000, 'Difference should be less than 41ms (${diff} ns)'
}

// Test format_time with nanoseconds
fn test_format_time_nanoseconds() {
	formatted := format_time(500)
	assert formatted == '500 ns', 'Should format as nanoseconds: ${formatted}'
}

// Test format_time with microseconds
fn test_format_time_microseconds() {
	formatted := format_time(5000)
	assert formatted.contains('μs') || formatted.contains('㎲'), 'Should format as microseconds: ${formatted}'
}

// Test format_time with milliseconds
fn test_format_time_milliseconds() {
	formatted := format_time(5_000_000)
	assert formatted.contains('ms'), 'Should format as milliseconds: ${formatted}'
}

// Test format_time with seconds
fn test_format_time_seconds() {
	formatted := format_time(i64(5_000_000_000))
	assert formatted.contains('secs'), 'Should format with seconds: ${formatted}'
	assert formatted == '5 secs', 'Should be "5 secs": ${formatted}'
}

// Test format_time with minutes
fn test_format_time_minutes() {
	formatted := format_time(i64(65_000_000_000)) // 65 seconds
	assert formatted.contains('min'), 'Should contain minutes: ${formatted}'
	assert formatted.contains('secs'), 'Should contain seconds: ${formatted}'
	assert formatted == '1 min 5 secs', 'Should be "1 min 5 secs": ${formatted}'
}

// Test format_time with hours
fn test_format_time_hours() {
	formatted := format_time(i64(3665_000_000_000)) // 1 hour, 1 minute, 5 seconds
	assert formatted.contains('hr'), 'Should contain hours: ${formatted}'
	assert formatted.contains('min'), 'Should contain minutes: ${formatted}'
	assert formatted.contains('secs'), 'Should contain seconds: ${formatted}'
}

// Test format_time with days
fn test_format_time_days() {
	formatted := format_time(i64(90065_000_000_000)) // 1 day, 1 hour, 1 minute, 5 seconds
	assert formatted.contains('day'), 'Should contain days: ${formatted}'
	assert formatted.contains('hr'), 'Should contain hours: ${formatted}'
}

// Test multiple start/stop cycles
fn test_timer_multiple_cycles() {
	mut timer := new_timer()

	// First cycle
	timer.start()
	sleep(millisecond.times(10))
	timer.stop()
	first_elapsed := timer.ns()

	// Second cycle
	timer.start()
	sleep(millisecond.times(10))
	timer.stop()
	second_elapsed := timer.ns()

	// Both should be reasonable times
	assert first_elapsed > 9_000_000, 'First elapsed should be at least 9ms'
	assert second_elapsed > 9_000_000, 'Second elapsed should be at least 9ms'
}

// Test timer while running (before stop)
fn test_timer_while_running() {
	mut timer := new_timer()
	timer.start()

	sleep(millisecond.times(10))
	elapsed1 := timer.ns()

	sleep(millisecond.times(10))
	elapsed2 := timer.ns()

	assert elapsed2 > elapsed1, 'Elapsed time should increase while running'

	timer.stop()
}

// Test timer frequency initialization
fn test_frequency_initialization() {
	timer1 := new_timer()
	timer2 := new_timer()

	// Both timers should have the same frequency
	assert timer1.freq == timer2.freq, 'All timers should share same frequency'
	assert timer1.freq > 0, 'Frequency should be positive'
}

// Benchmark test - measure overhead
fn test_timer_overhead() {
	mut timer := new_timer()

	// Start and immediately stop to measure overhead
	timer.start()
	timer.stop()

	overhead := timer.ns()

	// Overhead should be very small (less than 100 microseconds)
	assert overhead < 100_000, 'Timer overhead should be less than 100μs (${overhead} ns)'
	println('Timer overhead: ${overhead} ns')
}
