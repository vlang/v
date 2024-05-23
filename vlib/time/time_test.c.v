import time

fn test_tm_gmtoff() {
	$if windows {
		return
	} $else {
		rawtime := i64(0) // C.time_t{}

		C.time(&rawtime) // C.tm{}

		info := C.localtime(&rawtime)
		assert info.tm_gmtoff == time.now().unix() - time.utc().unix()
	}
}
