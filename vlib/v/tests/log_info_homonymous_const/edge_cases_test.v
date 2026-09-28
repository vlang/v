module main

import log
import net.websocket
import strings

struct Recorder {
mut:
	writes int
	last_buf string
}

fn (mut r Recorder) write(buf []u8) !int {
	r.writes++
	r.last_buf = buf.bytestr()
	return buf.len
}

fn mk() &log.Log {
	return &log.Log{}
}

const default_logger = mk()

// Edge case 1: Empty string, special characters, and huge string
fn test_log_boundary_strings() {
	mut rec := &Recorder{}
	mut lg := &log.Log{}
	lg.set_output_stream(rec)
	log.set_logger(lg)

	// 1. Empty string
	log.info('')
	assert rec.writes == 1

	// 2. Special characters: UTF-8, emojis, newlines, format specifiers, quotes
	special_msg := 'Hello 世界 🚀 🦀 \n\t\r "quotes" \'single\' %s %d %x %n'
	log.info(special_msg)
	assert rec.writes == 2
	assert rec.last_buf.contains('Hello 世界 🚀 🦀')

	// 3. Huge string (100,000 characters)
	huge_msg := strings.repeat(`X`, 100_000)
	log.info(huge_msg)
	assert rec.writes == 3
	assert rec.last_buf.len >= 100_000
}

// Edge case 2: All log levels with both websocket and user const present
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

// Edge case 3: Calling both log.info (global interface) and default_logger.info (local const) in the same run
fn test_coexistence_global_and_local_const_calls() {
	mut rec := &Recorder{}
	mut lg := &log.Log{}
	lg.set_output_stream(rec)
	log.set_logger(lg)

	// Calling via global dispatcher
	log.info('message via global log.info')
	assert rec.writes == 1

	// Calling via user const directly
	default_logger.info('message via user const directly')
	// default_logger is mk() (&log.Log{}), writing to stdout/stderr default, so rec.writes remains 1
	assert rec.writes == 1
}
