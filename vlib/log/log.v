module log

import os
import time
import term

pub enum Level {
	fatal = 1
	error
	warn
	info
	debug
}

fn tag(l Level) string {
	return match l {
		.fatal { term.red('FATAL') }
		.error { term.red('ERROR') }
		.warn  { term.yellow('WARN ') }
		.info  { term.white('INFO ') }
		.debug { term.blue('DEBUG') }
	}
}

interface Logger {
	fatal(s string)
	error(s string)
	warn(s string)
	info(s string)
	debug(s string)
}

pub struct Log {
mut:
	level            Level
	output_label     string
	ofile            os.File
	output_to_file   bool
pub mut:
	output_file_name string
}

pub fn (mut l Log) set_level(level Level) {
	l.level = level
}

pub fn (mut l Log) set_output_level(level Level) {
	l.level = level
}

pub fn (mut l Log) set_full_logpath(full_log_path string) {
	rlog_file := os.real_path( full_log_path )
	l.set_output_label( os.file_name( rlog_file ) )
	l.set_output_path( os.base_dir( rlog_file ) )
}

pub fn (mut l Log) set_output_label(label string){
	l.output_label = label
}

pub fn (mut l Log) set_output_path(output_file_path string) {
	if l.ofile.is_opened() { l.ofile.close() }
	l.output_to_file = true
	l.output_file_name = os.join_path( os.real_path( output_file_path ) , l.output_label )
	ofile := os.open_append( l.output_file_name ) or {
		panic('error while opening log file ${l.output_file_name} for appending')
	}
	l.ofile = ofile
}

pub fn (mut l Log) close() {
  l.ofile.close()
}

fn (mut l Log) log_file(s string, level Level) {
	timestamp := time.now().format_ss()
	e := tag(level)
	l.ofile.writeln('$timestamp [$e] $s')
}

fn (l &Log) log_cli(s string, level Level) {
	f := tag(level)
	t := time.now()
	println('[$f ${t.format_ss()}] $s')
}

fn (mut l Log) send_output(s &string, level Level) {
	if l.output_to_file {
		l.log_file(s, level)
	} else {
		l.log_cli(s, level)
	}
}

pub fn (mut l Log) fatal(s string){
	if l.level < .fatal { return }
	l.send_output(s, .fatal)
	l.ofile.close()
	panic('$l.output_label: $s')
}

pub fn (mut l Log) error(s string) {
	if l.level < .error { return }
	l.send_output(s, .error)
}

pub fn (mut l Log) warn(s string) {
	if l.level < .warn { return }
	l.send_output(s, .warn)
}

pub fn (mut l Log) info(s string) {
	if l.level < .info { return }
	l.send_output(s, .info)
}

pub fn (mut l Log) debug(s string) {
	if l.level < .debug { return }
	l.send_output(s, .debug)
}
