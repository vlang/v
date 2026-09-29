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

const default_logger = log.new_thread_safe_log()

// A homonymous `&ThreadSafeLog` const used to silently swallow `log.info`.
fn test_log_info_survives_homonymous_thread_safe_log_const() {
	mut rec := &Recorder{}
	mut lg := &log.Log{}
	lg.set_output_stream(rec)
	log.set_logger(lg)
	log.info('hello homonymous thread safe log const')
	assert rec.writes == 1
}
