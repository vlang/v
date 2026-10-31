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

fn setup_default_logger() &log.Log {
	return &log.Log{}
}

const default_logger = setup_default_logger()

fn test_log_info_survives_homonymous_user_const() {
	mut rec := &Recorder{}
	mut lg := &log.Log{}
	lg.set_output_stream(rec)
	log.set_logger(lg)
	log.info('hello homonymous user const')
	assert rec.writes == 1
}
