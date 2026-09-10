module bench

#insert "@VEXEROOT/vlib/v3/bench/peak_rss.h"

fn C.v3_bench_peak_rss_kb() i64

fn peak_rss_kb() i64 {
	peak := C.v3_bench_peak_rss_kb()
	if peak < 0 {
		return current_rss_kb()
	}
	return peak
}
