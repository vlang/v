module log

import os
import time

// json_escape escapes a string for use inside a JSON string literal:
// backslash, double quote and the C0 control characters get their standard
// JSON escapes, so field values and messages can never break the record.
fn json_escape(s string) string {
	mut b := []u8{cap: s.len + 8}
	for c in s.bytes() {
		match c {
			u8(34) {
				b << u8(92)
				b << u8(34)
			} // \"
			u8(92) {
				b << u8(92)
				b << u8(92)
			} // \\
			8 {
				b << u8(92)
				b << u8(98)
			} // \b
			9 {
				b << u8(92)
				b << u8(116)
			} // \t
			10 {
				b << u8(92)
				b << u8(110)
			} // \n
			12 {
				b << u8(92)
				b << u8(102)
			} // \f
			13 {
				b << u8(92)
				b << u8(114)
			} // \r
			else {
				if c < 0x20 {
					b << u8(92)
					b << u8(117)
					b << u8(48)
					b << u8(48)
					b << hex_digit(c >> 4)
					b << hex_digit(c & 0x0f)
				} else {
					b << c
				}
			}
		}
	}
	return b.bytestr()
}

// hex_digit returns the lowercase hex digit for a nibble 0..15.
fn hex_digit(n u8) u8 {
	if n < 10 {
		return u8(48) + n
	}
	return u8(97) + (n - 10)
}

// structured_line renders one JSON log record with stable field order
// (time, level, msg, then fields sorted by key), so output is diffable.
fn (l &Log) structured_line(s string, level Level, fields map[string]string) string {
	timestamp := l.time_format(if l.local_time { time.now() } else { time.utc() })
	mut parts := ['"time":"${json_escape(timestamp)}"', '"level":"${json_escape(level.str())}"',
		'"msg":"${json_escape(s)}"']
	mut keys := fields.keys()
	keys.sort()
	for k in keys {
		if k in ['time', 'level', 'msg'] {
			continue
		}
		parts << '"${json_escape(k)}":"${json_escape(fields[k])}"'
	}
	return '{' + parts.join(',') + '}'
}

// send_output_structured writes one JSON record to the configured target(s),
// mirroring send_output's target handling.
fn (mut l Log) send_output_structured(s string, level Level, fields map[string]string) {
	line := l.structured_line(s, level, fields)
	if l.output_target == .file || l.output_target == .both {
		unsafe {
			l.ofile.write_ptr(line.str, line.len)
			l.ofile.write_ptr(c'\n', 1)
		}
		if l.always_flush {
			l.flush()
		}
	}
	if l.output_target == .console || l.output_target == .both {
		msg := line + '\n'
		l.output_stream.write(msg.bytes()) or {}
		if l.always_flush {
			if mut l.output_stream is os.File {
				match l.output_stream.fd {
					1 { flush_stdout() }
					2 { flush_stderr() }
					else {}
				}
			}
		}
	}
}

// fatal_with logs a fatal record with structured fields and panics, like
// `fatal` does. Levels below the threshold are still fatal-panicking.
@[noreturn]
pub fn (mut l Log) fatal_with(s string, fields map[string]string) {
	if int(l.level) >= int(Level.fatal) {
		l.send_output_structured(s, .fatal, fields)
		l.ofile.close()
	}
	panic(l.output_label + ': ' + s)
}

// error_with logs an error record with structured fields, subject to the
// same level filtering as `error`.
pub fn (mut l Log) error_with(s string, fields map[string]string) {
	if int(l.level) < int(Level.error) {
		return
	}
	l.send_output_structured(s, .error, fields)
}

// warn_with logs a warning record with structured fields, subject to the
// same level filtering as `warn`.
pub fn (mut l Log) warn_with(s string, fields map[string]string) {
	if int(l.level) < int(Level.warn) {
		return
	}
	l.send_output_structured(s, .warn, fields)
}

// info_with logs an info record with structured fields, subject to the same
// level filtering as `info`. Output is a single-line JSON object with the
// reserved keys time, level and msg plus the given fields, in sorted key
// order. Fields using a reserved key are ignored; the plain-text methods (info, warn, ...) are unchanged.
pub fn (mut l Log) info_with(s string, fields map[string]string) {
	if int(l.level) < int(Level.info) {
		return
	}
	l.send_output_structured(s, .info, fields)
}

// debug_with logs a debug record with structured fields, subject to the same
// level filtering as `debug`.
pub fn (mut l Log) debug_with(s string, fields map[string]string) {
	if int(l.level) < int(Level.debug) {
		return
	}
	l.send_output_structured(s, .debug, fields)
}

// error_with logs a structured error record while holding the logger mutex.
pub fn (mut x ThreadSafeLog) error_with(s string, fields map[string]string) {
	if unsafe { x.mu == 0 } { return }
	x.mu.lock()
	defer { x.mu.unlock() }
	x.Log.error_with(s, fields)
}

// warn_with logs a structured warn record while holding the logger mutex.
pub fn (mut x ThreadSafeLog) warn_with(s string, fields map[string]string) {
	if unsafe { x.mu == 0 } { return }
	x.mu.lock()
	defer { x.mu.unlock() }
	x.Log.warn_with(s, fields)
}

// info_with logs a structured info record while holding the logger mutex.
pub fn (mut x ThreadSafeLog) info_with(s string, fields map[string]string) {
	if unsafe { x.mu == 0 } { return }
	x.mu.lock()
	defer { x.mu.unlock() }
	x.Log.info_with(s, fields)
}

// debug_with logs a structured debug record while holding the logger mutex.
pub fn (mut x ThreadSafeLog) debug_with(s string, fields map[string]string) {
	if unsafe { x.mu == 0 } { return }
	x.mu.lock()
	defer { x.mu.unlock() }
	x.Log.debug_with(s, fields)
}

// fatal_with logs a structured fatal record while holding the logger mutex and panics.
@[noreturn]
pub fn (mut x ThreadSafeLog) fatal_with(s string, fields map[string]string) {
	if unsafe { x.mu == 0 } { panic(s) }
	x.mu.lock()
	defer { x.mu.unlock() }
	x.Log.fatal_with(s, fields)
}
