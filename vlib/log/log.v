// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module log

import os
import time
import io

// TimeFormat define the log time string format, come from time/format.v
pub enum TimeFormat {
	tf_ss_micro      // YYYY-MM-DD HH:mm:ss.123456 (24h)
	tf_default       // YYYY-MM-DD HH:mm (24h)
	tf_ss            // YYYY-MM-DD HH:mm:ss (24h)
	tf_ss_milli      // YYYY-MM-DD HH:mm:ss.123 (24h)
	tf_ss_nano       // YYYY-MM-DD HH:mm:ss.123456789 (24h)
	tf_rfc3339       // YYYY-MM-DDTHH:mm:ss.123Z (24 hours, see https://www.rfc-editor.org/rfc/rfc3339.html)
	tf_rfc3339_micro // default, YYYY-MM-DDTHH:mm:ss.123456Z (24 hours, see https://www.rfc-editor.org/rfc/rfc3339.html)
	tf_rfc3339_nano  // YYYY-MM-DDTHH:mm:ss.123456789Z (24 hours, see https://www.rfc-editor.org/rfc/rfc3339.html)
	tf_hhmm          // HH:mm (24h)
	tf_hhmmss        // HH:mm:ss (24h)
	tf_hhmm12        // hh:mm (12h)
	tf_ymmdd         // YYYY-MM-DD
	tf_ddmmy         // DD.MM.YYYY
	tf_md            // MMM D
	tf_custom_format // 'MMMM Do YY N kk:mm:ss A' output like: January 1st 22 AD 13:45:33 PM
}

const stderr = os.stderr()

// Log represents a logging object
pub struct Log {
mut:
	level              Level = .debug
	output_label       string
	ofile              os.File
	output_target      LogTarget // output to console (stdout/stderr) or file or both.
	time_format        TimeFormat = .tf_rfc3339_micro
	custom_time_format string     = 'MMMM Do YY N kk:mm:ss A' // timestamp with custom format
	short_tag          bool
	local_time         bool // use local time or utc time in log
	always_flush       bool // flush after every single .fatal(), .error(), .warn(), .info(), .debug() call
	output_stream      io.Writer = stderr
pub mut:
	output_file_name string // log output to this file
}

// get_level gets the internal logging level.
pub fn (l &Log) get_level() Level {
	return l.level
}

// set_level sets the logging level to `level`. Messages for levels above it will skipped.
// For example, after calling log.set_level(.info), log.debug('message') will produce nothing.
// Call log.set_level(.disabled) to turn off the logging of all messages.
pub fn (mut l Log) set_level(level Level) {
	l.level = level
}

// set_full_logpath sets the output label and output path from `full_log_path`.
pub fn (mut l Log) set_full_logpath(full_log_path string) {
	rlog_file := os.real_path(full_log_path)
	l.set_output_label(os.file_name(rlog_file))
	l.set_output_path(os.dir(rlog_file))
}

// set_output_label sets the `label` for the output.
pub fn (mut l Log) set_output_label(label string) {
	l.output_label = label
}

// set_output_path sets the file to which output is logged to.
pub fn (mut l Log) set_output_path(output_file_path string) {
	if l.ofile.is_opened {
		l.ofile.close()
	}
	l.output_target = .file
	l.output_file_name = os.join_path(os.real_path(output_file_path), l.output_label)
	ofile := os.open_append(l.output_file_name) or {
		panic('error while opening log file ' + l.output_file_name + ' for appending')
	}
	l.ofile = ofile
}

// set_output_stream sets the output stream to write log e.g. stderr, stdout, etc.
pub fn (mut l Log) set_output_stream(stream io.Writer) {
	l.output_stream = stream
}

// log_to_console_too turns on logging to the console too, in addition to logging to a file.
// You have to call it *after* calling .set_output_path(output_file_path).
pub fn (mut l Log) log_to_console_too() {
	if l.output_target != .file {
		panic('log_to_console_too should be called *after* .set_output_path')
	}
	l.output_target = .both
}

// flush writes the log file content to disk.
pub fn (mut l Log) flush() {
	l.ofile.flush()
}

// close closes the log file.
pub fn (mut l Log) close() {
	l.ofile.close()
}

// reopen reopens the log file. Useful for log rotation.
// This does nothing if you are only writing to the console.
pub fn (mut l Log) reopen() ! {
	l.flush()
	if l.output_target == .file || l.output_target == .both {
		l.ofile.reopen(l.output_file_name, 'ab') or {
			return error_with_code('re-opening log file `${l.output_file_name}` for appending failed',
				1)
		}
	}
}

// log_file writes log line `s` with `level` to the log file.
fn (mut l Log) log_file(s string, level Level) {
	timestamp := l.time_format(if l.local_time { time.now() } else { time.utc() })
	e := tag_to_file(level, l.short_tag)

	unsafe {
		l.ofile.write_ptr(timestamp.str, timestamp.len)
		l.ofile.write_ptr(c' ', 1)

		l.ofile.write_ptr(c'[', 1)
		l.ofile.write_ptr(e.str, e.len)
		l.ofile.write_ptr(c']', 1)

		l.ofile.write_ptr(c' ', 1)
		l.ofile.write_ptr(s.str, s.len)

		l.ofile.write_ptr(c'\n', 1)
	}
	if l.always_flush {
		l.flush()
	}
}

// log_stream writes log line `s` with `level` to stderr or stderr depending on set output stream.
fn (mut l Log) log_stream(s string, level Level) {
	timestamp := l.time_format(if l.local_time { time.now() } else { time.utc() })
	tag := tag_to_console(level, l.short_tag)
	msg := '${timestamp} [${tag}] ${s}\n'
	arr := msg.bytes()
	l.output_stream.write(arr) or {}
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

// send_output writes log line `s` with `level` to either the log file or the console
// according to the value of the `.output_target` field.
pub fn (mut l Log) send_output(s &string, level Level) {
	if l.output_target == .file || l.output_target == .both {
		l.log_file(s, level)
	}
	if l.output_target == .console || l.output_target == .both {
		l.log_stream(s, level)
	}
}

// fatal logs line `s` via `send_output` if `Log.level` is greater than or equal to the `Level.fatal` category.
// Note that this method performs a panic at the end, even if log level is not enabled.
@[noreturn]
pub fn (mut l Log) fatal(s string) {
	if int(l.level) >= int(Level.fatal) {
		l.send_output(s, .fatal)
		l.ofile.close()
	}
	panic(l.output_label + ': ' + s)
}

// error logs line `s` via `send_output` if `Log.level` is greater than or equal to the `Level.error` category.
pub fn (mut l Log) error(s string) {
	if int(l.level) < int(Level.error) {
		return
	}
	l.send_output(s, .error)
}

// warn logs line `s` via `send_output` if `Log.level` is greater than or equal to the `Level.warn` category.
pub fn (mut l Log) warn(s string) {
	if int(l.level) < int(Level.warn) {
		return
	}
	l.send_output(s, .warn)
}

// info logs line `s` via `send_output` if `Log.level` is greater than or equal to the `Level.info` category.
pub fn (mut l Log) info(s string) {
	if int(l.level) < int(Level.info) {
		return
	}
	l.send_output(s, .info)
}

// debug logs line `s` via `send_output` if `Log.level` is greater than or equal to the `Level.debug` category.
pub fn (mut l Log) debug(s string) {
	if int(l.level) < int(Level.debug) {
		return
	}
	l.send_output(s, .debug)
}

// free frees the given Log instance
@[unsafe]
pub fn (mut f Log) free() {
	unsafe {
		f.output_label.free()
		f.ofile.close()
		f.output_file_name.free()
	}
}

// time_format return a timestamp string in the pre-defined format
fn (l Log) time_format(t time.Time) string {
	match l.time_format {
		.tf_rfc3339_micro { // YYYY-MM-DDTHH:mm:ss.123456Z (24 hours, see https://www.rfc-editor.org/rfc/rfc3339.html)
			return t.format_rfc3339_micro()
		}
		.tf_ss_micro { // YYYY-MM-DD HH:mm:ss.123456 (24h) default
			return t.format_ss_micro()
		}
		.tf_default { // YYYY-MM-DD HH:mm (24h)
			return t.format()
		}
		.tf_ss { // YYYY-MM-DD HH:mm:ss (24h)
			return t.format_ss()
		}
		.tf_ss_milli { // YYYY-MM-DD HH:mm:ss.123 (24h)
			return t.format_ss_milli()
		}
		.tf_ss_nano { // YYYY-MM-DD HH:mm:ss.123456789 (24h)
			return t.format_ss_nano()
		}
		.tf_rfc3339 { // YYYY-MM-DDTHH:mm:ss.123Z (24 hours, see https://www.rfc-editor.org/rfc/rfc3339.html)
			return t.format_rfc3339()
		}
		.tf_rfc3339_nano { // YYYY-MM-DDTHH:mm:ss.123456789Z (24 hours, see https://www.rfc-editor.org/rfc/rfc3339.html)
			return t.format_rfc3339_nano()
		}
		.tf_hhmm { // HH:mm (24h)
			return t.hhmm()
		}
		.tf_hhmmss { // HH:mm:ss (24h)
			return t.hhmmss()
		}
		.tf_hhmm12 { // hh:mm (12h)
			return t.hhmm12()
		}
		.tf_ymmdd { // YYYY-MM-DD
			return t.ymmdd()
		}
		.tf_ddmmy { // DD.MM.YYYY
			return t.ddmmy()
		}
		.tf_md { // MMM D
			return t.md()
		}
		.tf_custom_format { // 'MMMM Do YY N kk:mm:ss A' output like: January 1st 22 AD 13:45:33 PM
			return t.custom_format(l.custom_time_format)
		}
	}
}

// set_time_format will set the log time format to a pre-defined format
pub fn (mut l Log) set_time_format(f TimeFormat) {
	l.time_format = f
}

// set_always_flush called with true, will make the log flush after every single .fatal(), .error(), .warn(), .info(), .debug() call.
// That can be much slower, if you plan to do lots of frequent calls, but if your program exits early or crashes, your logs will be more complete.
pub fn (mut l Log) set_always_flush(should_flush_every_time bool) {
	l.always_flush = should_flush_every_time
}

// get_time_format will get the log time format
pub fn (l Log) get_time_format() TimeFormat {
	return l.time_format
}

// set_custom_time_format will set the log custom time format
// refer to time/custom_format() for more information
// eg. 'MMMM Do YY N kk:mm:ss A' output like: January 1st 22 AD 13:45:33 PM
pub fn (mut l Log) set_custom_time_format(f string) {
	l.time_format = .tf_custom_format
	l.custom_time_format = f
}

// get_custom_time_format will get the log custom time format
pub fn (l Log) get_custom_time_format() string {
	return l.custom_time_format
}

// set_short_tag will set the log tag to it's short version
// eg. '[FATAL]'=>'[F]', '[ERROR]'=>'[E]','[WARN ]'=>'[W]','[INFO ]'=>'[I]','[DEBUG]'=>'[D]'
pub fn (mut l Log) set_short_tag(enabled bool) {
	l.short_tag = enabled
}

// get_short_tag will get the log short tag enable state
pub fn (l Log) get_short_tag() bool {
	return l.short_tag
}

// set_local_time will set the log using local time
pub fn (mut l Log) set_local_time(enabled bool) {
	l.local_time = enabled
}

// get_local_time will get the log using local time state
pub fn (l Log) get_local_time() bool {
	return l.local_time
}

// use_stdout will restore the old behaviour of logging to stdout, instead of stderr.
// It will also silence the deprecation note in the transition period.
pub fn use_stdout() {
	mut l := ThreadSafeLog{}
	l.set_output_stream(os.stdout())
	set_logger(l)
}
