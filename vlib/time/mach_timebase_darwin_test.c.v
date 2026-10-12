import time as _

fn test_mach_timebase_info_returns_success_code() {
	$if macos {
		mut info := C.mach_timebase_info_data_t{}
		assert C.mach_timebase_info(&info) == i32(0)
		assert info.numer > 0
		assert info.denom > 0
	}
}
