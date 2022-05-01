import time

fn test_relative() {
	// past
	mut date := time.now()
	assert date.relative() == 'now'
	date = date.add_seconds(-61)
	assert date.relative() == '1 minute ago'
	assert date.relative_short() == '1m ago'
	date = date.add_seconds(-120)
	assert date.relative() == '3 minutes ago'
	assert date.relative_short() == '3m ago'
	date = date.add_seconds(-1 * time.seconds_per_hour)
	assert date.relative() == '1 hour ago'
	assert date.relative_short() == '1h ago'
	date = date.add_seconds(-5 * time.seconds_per_hour)
	assert date.relative() == '6 hours ago'
	assert date.relative_short() == '6h ago'
	date = date.add_seconds(-1 * time.seconds_per_day)
	assert date.relative() == '1 day ago'
	assert date.relative_short() == '1d ago'
	date = date.add_seconds(-4 * time.seconds_per_day)
	assert date.relative() == '5 days ago'
	assert date.relative_short() == '5d ago'
	date = time.now().add_seconds(-75 * time.seconds_per_day)
	assert date.relative() == 'last ${date.custom_format('MMM')} ${date.custom_format('D')}'
	assert date.relative_short() == '75d ago'
	date = time.now().add_seconds(-400 * time.seconds_per_day)
	assert date.relative() == '1 year ago'
	assert date.relative_short() == '1y ago'

	// future
	date = time.now()
	date = date.add_seconds(61)
	assert date.relative() == 'in 1 minute'
	assert date.relative_short() == 'in 1m'
	date = date.add_seconds(120)
	assert date.relative() == 'in 3 minutes'
	assert date.relative_short() == 'in 3m'
	date = date.add_seconds(1 * time.seconds_per_hour)
	assert date.relative() == 'in 1 hour'
	assert date.relative_short() == 'in 1h'
	date = date.add_seconds(5 * time.seconds_per_hour)
	assert date.relative() == 'in 6 hours'
	assert date.relative_short() == 'in 6h'
	date = date.add_seconds(time.seconds_per_day)
	assert date.relative() == 'in 1 day'
	assert date.relative_short() == 'in 1d'
	date = date.add_seconds(4 * time.seconds_per_day)
	assert date.relative() == 'in 5 days'
	assert date.relative_short() == 'in 5d'
	date = time.now().add_seconds(75 * time.seconds_per_day)
	assert date.relative() == 'on ${date.custom_format('MMM')} ${date.custom_format('D')}'
	assert date.relative_short() == 'in 75d'
	date = time.now().add_seconds(400 * time.seconds_per_day)
	assert date.relative() == 'in 1 year'
	assert date.relative_short() == 'in 1y'
}
