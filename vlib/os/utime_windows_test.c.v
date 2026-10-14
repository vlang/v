module os

fn test_windows_filetime_from_unix_seconds() {
	for seconds in [i64(-1), i64(0), i64(2_147_483_648), i64(2_306_102_494)] {
		filetime := windows_filetime_from_unix_seconds(seconds)!
		assert windows_filetime_to_unix_seconds(filetime) == seconds
	}
	first := windows_filetime_from_unix_seconds(-windows_filetime_unix_epoch_seconds + 1)!
	assert first.dw_low_date_time == u32(windows_filetime_ticks_per_second)
	assert first.dw_high_date_time == 0
	max_seconds := i64(u64(0x7fff_ffff_ffff_ffff) / windows_filetime_ticks_per_second) -
		windows_filetime_unix_epoch_seconds
	last := windows_filetime_from_unix_seconds(max_seconds)!
	assert windows_filetime_to_unix_seconds(last) == max_seconds
	assert last.dw_high_date_time == u32(0x7fff_ffff)
	for invalid in [-windows_filetime_unix_epoch_seconds, -windows_filetime_unix_epoch_seconds - 1,
		max_seconds + 1, i64(~u64(0) / windows_filetime_ticks_per_second) -
			windows_filetime_unix_epoch_seconds, i64(0x7fff_ffff_ffff_ffff)] {
		if _ := windows_filetime_from_unix_seconds(invalid) {
			assert false, 'invalid Windows timestamp must be rejected'
		} else {
			assert err.code() == int(C.ERROR_INVALID_PARAMETER)
		}
	}
}
