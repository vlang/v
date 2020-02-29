module log

import (
	os
	time
	term
	filepath
)

pub enum LogLevel {
	fatal = 1
	error
	warn
	info
	debug
}

fn tag(l LogLevel) string {
	return match l {
		.fatal { term.red('FATAL') }
		.error { term.red('ERROR') }
		.warn  { term.yellow('WARN ') }
		.info  { term.white('INFO ') }
		.debug { term.blue('DEBUG') }
		else { '     ' }
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
	level            LogLevel
	output_label     string
	ofile            os.File
	output_to_file   bool
pub mut:
	output_file_name string
}

pub fn (l mut Log) set_level(level int) {
	l.level = match level {
		FATAL { LogLevel.fatal }
		ERROR { LogLevel.error }
		WARN { LogLevel.warn }
		INFO { LogLevel.info }
		DEBUG { LogLevel.debug }
		else { .debug }
	}
}

pub fn (l mut Log) set_output_level(level LogLevel) {
	l.level = level
}

pub fn (l mut Log) set_full_logpath(full_log_path string) {
	rlog_file := os.realpath( full_log_path )
	l.set_output_label( filepath.filename( rlog_file ) )
	l.set_output_path( filepath.basedir( rlog_file ) )
}

pub fn (l mut Log) set_output_label(label string){
	l.output_label = label
}

pub fn (l mut Log) set_output_path(output_file_path string) {
	if l.ofile.is_opened() { l.ofile.close() }
	l.output_to_file = true
	l.output_file_name = filepath.join( os.realpath( output_file_path ) , l.output_label )
	ofile := os.open_append( l.output_file_name ) or {
		panic('error while opening log file ${l.output_file_name} for appending')
	}
	l.ofile = ofile
}

pub fn (l mut Log) close() {
  l.ofile.close()
}

fn (l mut Log) log_file(s string, level LogLevel) {
	timestamp := time.now().format_ss()
	e := tag(level)
	l.ofile.writeln('$timestamp [$e] $s')
}

fn (l &Log) log_cli(s string, level LogLevel) {
	f := tag(level)
	t := time.now()
	println('[$f ${t.format_ss()}] $s')
}

fn (l mut Log) send_output(s &string, level LogLevel) {
	if l.output_to_file {
		l.log_file(s, level)
	} else {
		l.log_cli(s, level)
	}
}

pub fn (l mut Log) fatal(s string){
	if l.level < .fatal { return }
	l.send_output(s, .fatal)
	l.ofile.close()
	panic('$l.output_label: $s')
}

pub fn (l mut Log) error(s string) {
	if l.level < .error { return }
	l.send_output(s, .error)
}

pub fn (l mut Log) warn(s string) {
	if l.level < .warn { return }
	l.send_output(s, .warn)
}

pub fn (l mut Log) info(s string) {
	if l.level < .info { return }
	l.send_output(s, .info)
}

pub fn (l mut Log) debug(s string) {
	if l.level < .debug { return }
	l.send_output(s, .debug)
}

