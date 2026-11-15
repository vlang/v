module time

#flag @VEXEROOT/vlib/time/testdata/windows_zoneinfo/lookup.c

$if !windows {
	#flag -I @VEXEROOT/vlib/time/testdata/windows_zoneinfo
}

fn C.v_time_test_windows_zoneinfo() i32

fn test_windows_zoneinfo_looks_up_optional_apis() {
	assert C.v_time_test_windows_zoneinfo() == 0
}
