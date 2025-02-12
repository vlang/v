import time

fn test_tm_gmtoff() {
	$if windows {
		return
	} $else {
		rawtime := i64(0) // C.time_t{}

		C.time(&rawtime) // C.tm{}

		info := C.localtime(&rawtime)
		t1 := time.now()
		t2 := time.utc()
		dump(t1)
		dump(t2)
		dump(t1.nanosecond)
		dump(t2.nanosecond)
		diff := t1.unix() - t2.unix()
		dump(diff)
		dump(info.tm_gmtoff)
		assert info.tm_gmtoff == diff
	}
}
