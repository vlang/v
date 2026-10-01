module main

import log
import net.websocket

struct Recorder {
mut:
	writes int
}

fn (mut r Recorder) write(buf []u8) !int {
	r.writes++
	return buf.len
}

// net.websocket's `const default_logger = &log.Log{}` must not hijack log.info (#29026).
fn test_log_info_survives_websocket_default_logger_const() {
	_ := websocket.ClientState{}
	mut rec := &Recorder{}
	mut lg := &log.Log{}
	lg.set_output_stream(rec)
	log.set_logger(lg)
	log.info('hello websocket import')
	assert rec.writes == 1
}
