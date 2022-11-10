module time

fn test_days_from_unix_epoch() {
	assert days_from_unix_epoch(1970, 1, 1) == 0
	assert days_from_unix_epoch(2022, 11, 10) == 19306
}
