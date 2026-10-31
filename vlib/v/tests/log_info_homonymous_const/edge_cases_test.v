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

fn setup_default_logger() &log.Log {
	return &log.Log{}
}

const default_logger = setup_default_logger()

fn test_all_log_levels_with_websocket_and_user_const() {
	_ := websocket.ClientState{}
	mut rec := &Recorder{}
	mut lg := &log.Log{}
	lg.set_output_stream(rec)
	log.set_logger(lg)

	log.set_level(.debug)
	log.debug('debug level message')
	log.info('info level message')
	log.warn('warn level message')
	log.error('error level message')

	assert rec.writes == 4
}

fn test_coexistence_global_and_local_const_calls() {
	mut rec := &Recorder{}
	mut lg := &log.Log{}
	lg.set_output_stream(rec)
	log.set_logger(lg)

	log.info('message via global log.info')
	assert rec.writes == 1

	// default_logger writes to stdout/stderr, so rec.writes stays 1.
	default_logger.info('message via user const directly')
	assert rec.writes == 1
}
