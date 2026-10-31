module bench

#flag @VMODROOT/vlib/v/bench/peak_rss.o

fn C.v3_bench_peak_rss_kb() i64

fn peak_rss_kb() i64 {
	peak := C.v3_bench_peak_rss_kb()
	if peak < 0 {
		return current_rss_kb()
	}
	return peak
}
