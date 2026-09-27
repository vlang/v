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

fn mk() &log.Log {
	return &log.Log{}
}

const default_logger = mk()

// `log.info` must keep interface dispatch on the `log.default_logger` global
// even when a same-named user const exists (github.com/vlang/v/issues/29026).
fn test_log_info_survives_homonymous_user_const() {
	mut rec := &Recorder{}
	mut lg := &log.Log{}
	lg.set_output_stream(rec)
	log.set_logger(lg)
	log.info('hello homonymous user const')
	assert rec.writes == 1
}
