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

const my_logger = &log.Log{}

fn test_log_info_with_different_name_const() {
	mut rec := &Recorder{}
	mut lg := &log.Log{}
	lg.set_output_stream(rec)
	log.set_logger(lg)
	log.info('hello different name const')
	assert rec.writes == 1
	assert my_logger != unsafe { nil }
}
