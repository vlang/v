// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module log

import os
import time
import term

// Level defines possible log levels used by `Log`
pub enum Level {
	disabled = 0
	fatal
	error
	warn
	info
	debug
}

// LogTarget defines possible log targets
pub enum LogTarget {
	console
	file
	both
}

// tag_to_cli returns the tag for log level `l` as a colored string.
fn tag_to_cli(l Level) string {
	return match l {
		.disabled { '' }
		.fatal { term.red('FATAL') }
		.error { term.red('ERROR') }
		.warn { term.yellow('WARN ') }
		.info { term.white('INFO ') }
		.debug { term.blue('DEBUG') }
	}
}

// tag_to_file returns the tag for log level `l` as a string.
fn tag_to_file(l Level) string {
	return match l {
		.disabled { '     ' }
		.fatal { 'FATAL' }
		.error { 'ERROR' }
		.warn { 'WARN ' }
		.info { 'INFO ' }
		.debug { 'DEBUG' }
	}
}

// level_from_tag returns the log level from the given string if matches.
pub fn level_from_tag(tag string) ?Level {
	return match tag {
		'DISABLED' { Level.disabled }
		'FATAL' { Level.fatal }
		'ERROR' { Level.error }
		'WARN' { Level.warn }
		'INFO' { Level.info }
		'DEBUG' { Level.debug }
		else { none }
	}
}

// Logger is an interface that describes a generic Logger
pub interface Logger {
mut:
	fatal(s string)
	error(s string)
	warn(s string)
	info(s string)
	debug(s string)
}

// Log represents a logging object
pub struct Log {
mut:
	level         Level
	output_label  string
	ofile         os.File
	output_target LogTarget // output to console (stdout/stderr) or file or both.
pub mut:
	output_file_name string // log output to this file
}

// get_level gets the internal logging level.
pub fn (mut l Log) get_level() Level {
	return l.level
}

// set_level sets the internal logging to `level`.
pub fn (mut l Log) set_level(level Level) {
	l.level = level
}

// set_output_level sets the internal logging output to `level`.
pub fn (mut l Log) set_output_level(level Level) {
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
		panic('error while opening log file $l.output_file_name for appending')
	}
	l.ofile = ofile
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

// log_file writes log line `s` with `level` to the log file.
fn (mut l Log) log_file(s string, level Level) {
	timestamp := time.now().format_ss()
	e := tag_to_file(level)
	l.ofile.writeln('$timestamp [$e] $s') or { panic(err) }
}

// log_cli writes log line `s` with `level` to stdout.
fn (l &Log) log_cli(s string, level Level) {
	timestamp := time.now().format_ss()
	e := tag_to_cli(level)
	println('$timestamp [$e] $s')
}

// send_output writes log line `s` with `level` to either the log file or the console
// according to the value of the `.output_target` field.
pub fn (mut l Log) send_output(s &string, level Level) {
	if l.output_target == .file || l.output_target == .both {
		l.log_file(s, level)
	}
	if l.output_target == .console || l.output_target == .both {
		l.log_cli(s, level)
	}
}

// fatal logs line `s` via `send_output` if `Log.level` is greater than or equal to the `Level.fatal` category.
// Note that this method performs a panic at the end, even if log level is not enabled.
pub fn (mut l Log) fatal(s string) {
	if int(l.level) >= int(Level.fatal) {
		l.send_output(s, .fatal)
		l.ofile.close()
	}
	panic('$l.output_label: $s')
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
