module testing

import term
import time

// ExtendedReporter implements the interface testing.Reporter.
// It is used by default by `v test .`
// It was extracted by the original non customiseable output implementation directly in cmd/tools/modules/testing/common.v
pub struct ExtendedReporter {
mut:
	compilation_duration map[string]time.Duration
	run_duration         map[string]time.Duration
	file_size            map[string]string
}

pub fn (r ExtendedReporter) session_start(message string, mut ts TestSession) {
	header(message)
	///	eprintln('Status           Compilation        Runtime      Total    File')
}

pub fn (r ExtendedReporter) session_stop(message string, mut ts TestSession) {
	println(ts.benchmark.total_message(message))
}

// the most general form; it may be useful for other reporters
// in the normal one, it currently does nothing
pub fn (mut r ExtendedReporter) report(index int, message &LogMessage) {
	if message.kind == .compilation_end {
		r.compilation_duration[message.file] = message.took
		r.file_size[message.file] = message.meta['exe_size']
	}
	if message.kind == .run_end {
		r.run_duration[message.file] = message.took
	}
	// eprintln('> ${@METHOD} index: $index | message: $message')
}

pub fn (r ExtendedReporter) report_stop() {
	// eprintln('> ${@METHOD}')
	eprintln('')
}

//// TODO: reconsider if these should be public:

pub fn (r ExtendedReporter) progress(index int, message string, log_message &LogMessage) {
	eprintln(message)
}

// in progress mode, the last line will be rewritten many times, and does not end with \n
// the \n will be printed just once when some progress has been made.
pub fn (r ExtendedReporter) update_last_line(index int, message string, log_message &LogMessage) {
	print('\r${empty}\r${message}')
	flush_stdout()
}

pub fn (r ExtendedReporter) update_last_line_and_move_to_next(index int, message string, log_message &LogMessage) {
	// the last \n is needed, so SKIP/FAIL messages
	// will not get overwritten by the OK ones
	eprint('\r${empty}\r${message}\n')
}

pub fn (r ExtendedReporter) message(index int, message string, log_message &LogMessage) {
	cdur := r.compilation_duration[log_message.file]
	rdur := r.run_duration[log_message.file]
	// diff := time.Duration(log_message.took - (cdur + rdur))
	nmessage := message.replace_each([' ms ', ' ms, ', ' [', ', [']).replace('] ', '], ${r.file_size[log_message.file]:8} B, ${f64(cdur.microseconds()) / 1000.0:9} ms c,   ${f64(rdur.microseconds()) / 1000.0:8} ms r, ')
	eprintln(nmessage)
}

pub fn (r ExtendedReporter) divider() {
	eprintln(term.h_divider('-'))
}

//

pub fn (r ExtendedReporter) worker_threads_start(files []string, mut ts TestSession) {
	// eprintln('> ${@METHOD}')
}

pub fn (r ExtendedReporter) worker_threads_finish(mut ts TestSession) {
	// eprintln('> ${@METHOD}')
}

pub fn (r ExtendedReporter) list_of_failed_commands(failed_cmds []string) {
	for i, cmd in failed_cmds {
		eprintln(term.failed('Failed command ${i + 1}:') + '    ${cmd}')
	}
}
