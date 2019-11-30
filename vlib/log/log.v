module log

import os
import time
import term

pub enum LogLevel {
	fatal
	error
	warning
	info
	debug
}

fn tag(l LogLevel) string {
	return match l {
		.fatal { term.red('F') }
		.error { term.red('E') }
		.warning { term.yellow('W') }
		.info { term.white('I') }
		.debug { term.blue('D') }
		else { ' ' }
	}
}

pub const (
	FATAL = 1
	ERROR = 2
	WARN  = 3
	INFO  = 4
	DEBUG = 5
)

interface Logger {
	fatal(s string)
	error(s string)
	warn(s string)
	info(s string)
	debug(s string)
}

pub struct Log {
	mut:
	level LogLevel
	output_label string
	output_to_file bool
	output_file_name string
	ofile os.File
}

pub fn (l mut Log) set_level(level int){
	l.level = match level {
		FATAL { LogLevel.fatal }
		ERROR { LogLevel.error }
		WARN { LogLevel.warning }
		INFO { LogLevel.info }
		DEBUG { LogLevel.debug }
		else { .debug }
	}
}

pub fn (l mut Log) set_output_level(level LogLevel){
	l.level = level
}

pub fn (l mut Log) set_output_file(output_file_path string) {
	l.output_file_name = output_file_path
	ofile := os.open_append( output_file_path ) or {
		panic('error opening log file $output_file_path for appending')
	}
	l.ofile = ofile  
}

fn (l Log) log_file(s string, level LogLevel) {
	timestamp := time.now().format_ss()
	e := tag(level)
	l.ofile.writeln('$timestamp [$e] $s')
}

fn (l Log) log_cli(s string, level LogLevel) {
	f := tag(level)
	t := time.now()
	println('[$f ${t.format_ss()}] $s')
}

pub fn (l Log) fatal(s string){
	if l.level == .fatal {
		if l.output_to_file {
			l.log_file(s, .fatal)
		} else {
			l.log_cli(s, .fatal)
		}
		panic('$l.output_label: $s')
	}
}

pub fn (l Log) error(s string){
	if l.level in [.info, .debug, .warning, .error] {
		if l.output_to_file {
			l.log_file(s, .error)
		} else {
			l.log_cli(s, .error)
		}

	}
}

pub fn (l Log) warn(s string){
	if l.level in [.info, .debug, .warning] {
		if l.output_to_file {
			l.log_file(s, .warning)
		} else {
			l.log_cli(s, .warning)
		}
	}
}

pub fn (l Log) info(s string){
	if l.level in [.info, .debug] {
		if l.output_to_file {
			l.log_file(s, .info)
		} else {
			l.log_cli(s, .info)
		}
	}
}

pub fn (l Log) debug(s string){
	if l.level != .debug {
		return
	}
	if l.output_to_file {
		l.log_file(s, .debug)
	} else {
		l.log_cli(s, .debug)
	}
}
