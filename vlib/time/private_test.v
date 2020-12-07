// tests that use and test private functions
module time

// test the old behavor is same as new, the unix time should always be local time
fn test_new_is_same_as_old_for_all_platforms() {
	t := C.time(0)
	tm := C.localtime(&t)
	old_time := convert_ctime(tm, 0)
	new_time := now()
	diff := new_time.unix - old_time.unix
	// could in very rare cases be that the second changed between calls
	assert (diff >= 0 && diff <= 1) == true
}
