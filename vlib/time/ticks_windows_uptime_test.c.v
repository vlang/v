module time

#flag @VEXEROOT/vlib/time/testdata/windows_ticks/uptime.c

$if !windows {
	#flag -I @VEXEROOT/vlib/time/testdata/windows_ticks
}

fn C.v_time_test_windows_uptime() int

fn test_windows_ticks_preserves_uptime_past_32_bits() {
	assert C.v_time_test_windows_uptime() == 0
}
