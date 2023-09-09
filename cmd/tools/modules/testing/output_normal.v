module testing

import term
import time

pub const empty = term.header(' ', ' ')

// NormalReporter implements the interface testing.Reporter.
// It is used by default by `v test .`
// It was extracted by the original non customiseable output implementation directly in cmd/tools/modules/testing/common.v
pub struct NormalReporter {
mut:
	compilation_duration map[string]time.Duration
	run_duration         map[string]time.Duration
}

pub fn (r NormalReporter) session_start(message string, mut ts TestSession) {
	header(message)
	eprintln('Status             Total       Compilation        Runtime                 Test file')
}

pub fn (r NormalReporter) session_stop(message string, mut ts TestSession) {
	println(ts.benchmark.total_message(message))
}

// the most general form; it may be useful for other reporters
// in the normal one, it currently does nothing
pub fn (mut r NormalReporter) report(index int, message &LogMessage) {
	if message.kind == .compilation_end {
		r.compilation_duration[message.file] = message.took
	}
	if message.kind == .run_end {
		r.run_duration[message.file] = message.took
	}
	// eprintln('> ${@METHOD} index: $index | message: $message')
}

pub fn (r NormalReporter) report_stop() {
	// eprintln('> ${@METHOD}')
	eprintln('')
}

//// TODO: reconsider if these should be public:

pub fn (r NormalReporter) progress(index int, message string, log_message &LogMessage) {
	eprintln(message)
}

// in progress mode, the last line will be rewritten many times, and does not end with \n
// the \n will be printed just once when some progress has been made.
pub fn (r NormalReporter) update_last_line(index int, message string, log_message &LogMessage) {
	print('\r${testing.empty}\r${message}')
	flush_stdout()
}

pub fn (r NormalReporter) update_last_line_and_move_to_next(index int, message string, log_message &LogMessage) {
	// the last \n is needed, so SKIP/FAIL messages
	// will not get overwritten by the OK ones
	eprint('\r${testing.empty}\r${message}\n')
}

pub fn (r NormalReporter) message(index int, message string, log_message &LogMessage) {
	cdur := r.compilation_duration[log_message.file]
	rdur := r.run_duration[log_message.file]
	nmessage := message.replace(' ms ', ' ms ~ ${cdur:10}   +   ${rdur:10}   ')
	eprintln(nmessage)
}

pub fn (r NormalReporter) divider() {
	eprintln(term.h_divider('-'))
}

//

pub fn (r NormalReporter) worker_threads_start(files []string, mut ts TestSession) {
	// eprintln('> ${@METHOD}')
}

pub fn (r NormalReporter) worker_threads_finish(mut ts TestSession) {
	// eprintln('> ${@METHOD}')
}

pub fn (r NormalReporter) list_of_failed_commands(failed_cmds []string) {
	for i, cmd in failed_cmds {
		eprintln(term.failed('Failed command ${i + 1}:') + '    ${cmd}')
	}
}
