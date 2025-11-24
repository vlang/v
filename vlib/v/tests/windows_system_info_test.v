// vtest build: windows

fn test_windows_system_info() {
	x := C.SYSTEM_INFO{}
	C.GetSystemInfo(&x)
	dump(x)
	assert x.wProcessorArchitecture != 0
}
