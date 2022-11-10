module time

fn test_days_from_unix_epoch() {
	time_test := Time{
		year: 2000
		month: 5
		day: 10
		hour: 22
		minute: 11
		second: 3
		microsecond: 100
	}
	assert time_test.days_from_unix_epoch() == 11087

	assert date_from_days_after_unix_epoch(11087).year == 2000
	assert date_from_days_after_unix_epoch(11087).month == 5
	assert date_from_days_after_unix_epoch(11087).day == 10

	assert days_from_unix_epoch(1970, 1, 1) == 0

	assert days_from_unix_epoch(1970, 2, 1) == 31
	assert days_from_unix_epoch(1970, 3, 1) == 59

	assert days_from_unix_epoch(2022, 11, 10) == 19306

	assert date_from_days_after_unix_epoch(1).year == 1970
	assert date_from_days_after_unix_epoch(1).month == 1
	assert date_from_days_after_unix_epoch(1).day == 2

	assert date_from_days_after_unix_epoch(31).year == 1970
	assert date_from_days_after_unix_epoch(31).month == 2
	assert date_from_days_after_unix_epoch(31).day == 1

	assert date_from_days_after_unix_epoch(59).year == 1970
	assert date_from_days_after_unix_epoch(59).month == 3
	assert date_from_days_after_unix_epoch(59).day == 1
}
