module time

fn test_until_future() {
	target := now().add(hour + 123 * nanosecond)
	before := now()
	duration := until(target)
	after := now()
	assert duration <= target - before
	assert duration >= target - after
}

fn test_until_past() {
	target := now().add(-hour - 123 * nanosecond)
	before := now()
	duration := until(target)
	after := now()
	assert duration <= target - before
	assert duration >= target - after
	assert duration < 0
}

fn test_until_utc() {
	target := utc().add(hour)
	before := now()
	duration := until(target)
	after := now()
	assert duration <= target - before
	assert duration >= target - after
}

fn test_until_current_time() {
	target := now()
	duration := until(target)
	after := now()
	assert duration <= 0
	assert duration >= target - after
}
