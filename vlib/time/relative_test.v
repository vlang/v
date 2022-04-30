import time

fn test_relative() {
	// past
	mut date := time.now()
	assert date.relative() == 'now'
	date = date.add_seconds(-61)
	assert date.relative() == '1 minute ago'
	date = date.add_seconds(-120)
	assert date.relative() == '3 minutes ago'
	date = date.add_seconds(-1 * time.seconds_per_hour)
	assert date.relative() == '1 hour ago'
	date = date.add_seconds(-5 * time.seconds_per_hour)
	assert date.relative() == '6 hours ago'
	date = date.add_seconds(-1 * time.seconds_per_day)
	assert date.relative() == '1 day ago'
	date = date.add_seconds(-4 * time.seconds_per_day)
	assert date.relative() == '5 days ago'
	date = time.now().add_seconds(-75 * time.seconds_per_day)
	assert date.relative() == 'last ${date.custom_format('MMM')} ${date.custom_format('D')}'
	date = time.now().add_seconds(-400 * time.seconds_per_day)
	assert date.relative() == '1 year ago'

	// future
	date = time.now()
	date = date.add_seconds(61)
	assert date.relative() == 'in 1 minute'
	date = date.add_seconds(120)
	assert date.relative() == 'in 3 minutes'
	date = date.add_seconds(1 * time.seconds_per_hour)
	assert date.relative() == 'in 1 hour'
	date = date.add_seconds(5 * time.seconds_per_hour)
	assert date.relative() == 'in 6 hours'
	date = date.add_seconds(time.seconds_per_day)
	assert date.relative() == 'in 1 day'
	date = date.add_seconds(4 * time.seconds_per_day)
	assert date.relative() == 'in 5 days'
	date = time.now().add_seconds(75 * time.seconds_per_day)
	assert date.relative() == 'on ${date.custom_format('MMM')} ${date.custom_format('D')}'
	date = time.now().add_seconds(400 * time.seconds_per_day)
	assert date.relative() == 'in 1 year'
}
