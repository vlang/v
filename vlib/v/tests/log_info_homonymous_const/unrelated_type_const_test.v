module main

import log

struct Recorder {
mut:
	writes int
}

fn (mut r Recorder) write(buf []u8) !int {
	r.writes++
	return buf.len
}

struct MyLog {}

const default_logger = MyLog{}

// The const has no `info`; `log.info` must still dispatch on the log global.
fn test_log_info_survives_homonymous_unrelated_const() {
	mut rec := &Recorder{}
	mut lg := &log.Log{}
	lg.set_output_stream(rec)
	log.set_logger(lg)
	log.info('hello homonymous unrelated const')
	assert rec.writes == 1
}
