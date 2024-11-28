import time

fn test_duration_str() {
	assert time.Duration(1 * time.nanosecond).str() == '1ns'
	assert time.Duration(999 * time.nanosecond).str() == '999ns'
	assert time.Duration(1000 * time.nanosecond).str() == '1.000us'

	assert time.Duration(1 * time.microsecond).str() == '1.000us'
	assert time.Duration(999 * time.microsecond).str() == '999.000us'
	assert time.Duration(1000 * time.microsecond).str() == '1.000ms'

	assert time.Duration(1 * time.second).str() == '1.000s'
	assert time.Duration(999 * time.second).str() == '16:39.000'
	assert time.Duration(1000 * time.second).str() == '16:40.000'

	assert time.Duration(1 * time.minute).str() == '1:00.000'
	assert time.Duration(999 * time.minute).str() == '16:39:00'
	assert time.Duration(1000 * time.minute).str() == '16:40:00'

	assert time.Duration(1 * time.hour).str() == '1:00:00'
	assert time.Duration(999 * time.hour).str() == '999:00:00'
	assert time.Duration(1000 * time.hour).str() == '1000:00:00'

	assert time.Duration(1 * time.microsecond + 7 * time.nanosecond).str() == '1.007us'

	assert time.Duration(1 * time.second + 5 * time.nanosecond).str() == '1.000s'
	assert time.Duration(1 * time.second + 5 * time.microsecond).str() == '1.000s'
	assert time.Duration(1 * time.second + 5 * time.millisecond).str() == '1.005s'

	assert time.Duration(1 * time.hour + 5 * time.millisecond).str() == '1:00:00'
	assert time.Duration(1 * time.hour + 5 * time.second).str() == '1:00:05'
	assert time.Duration(168 * time.hour + 5 * time.minute + 7 * time.second).str() == '168:05:07'
}

fn test_negative_duration() {
	now := time.parse('2000-01-01 10:00:00')!
	later := time.parse('2000-01-01 11:00:00')!
	duration := now - later
	assert time.Duration(-1 * time.hour) == duration
	assert duration.str() == '-1:00:00'
}

fn test_duration_debug() {
	assert time.Duration(1 * time.nanosecond).debug() == 'Duration: 1ns'
	assert time.Duration(169 * time.hour + 5 * time.minute + 7 * time.second).debug() == 'Duration: 7days, 1h, 5m, 7s'
	assert (-time.Duration(169 * time.hour + 5 * time.minute + 7 * time.second)).debug() == 'Duration: - 7days, 1h, 5m, 7s'
}
