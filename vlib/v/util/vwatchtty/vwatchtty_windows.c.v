module vwatchtty

// process_group returns zero on Windows, where terminal process groups are not used.
pub fn process_group() int {
	return 0
}

// set_foreground_process_group is a no-op on Windows, where terminal process groups are not used.
pub fn set_foreground_process_group(_ int, _ int) bool {
	return false
}

// restore_foreground_process_group is a no-op on Windows.
pub fn restore_foreground_process_group(_ int) {
}
