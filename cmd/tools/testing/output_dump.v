module testing

// DumpReporter implements the interface testing.Reporter.
// It is used by `v -test-runner dump test .`
pub struct DumpReporter {
mut:
	files []string
}

//

pub fn (mut r DumpReporter) worker_threads_start(files []string, mut ts TestSession) {
	eprintln('> ${@METHOD} | files: ${files}')
	r.files = files
}

pub fn (r DumpReporter) worker_threads_finish(mut ts TestSession) {
	eprintln('> ${@METHOD}')
}

//

pub fn (r DumpReporter) session_start(message string, mut ts TestSession) {
	eprintln('> ${@METHOD} | message: ${message}')
	// dump(ts)
}

pub fn (r DumpReporter) session_stop(message string, mut ts TestSession) {
	eprintln('> ${@METHOD} | message: ${message}')
}

//

pub fn (r DumpReporter) report(index int, message LogMessage) {
	eprintln('> ${@METHOD} | index: ${index} | message: ${message}')
}

pub fn (r DumpReporter) report_stop() {
	eprintln('> ${@METHOD}')
}

pub fn (r DumpReporter) progress(index int, message string) {
	eprintln('> ${@METHOD} | index: ${index} | message: ${message}')
}

pub fn (r DumpReporter) update_last_line(index int, message string) {
	eprintln('> ${@METHOD} | index: ${index} | message: ${message}')
}

pub fn (r DumpReporter) update_last_line_and_move_to_next(index int, message string) {
	eprintln('> ${@METHOD} | index: ${index} | message: ${message}')
}

pub fn (r DumpReporter) message(index int, message string) {
	eprintln('> ${@METHOD} | index: ${index} | message: ${message}')
}

pub fn (r DumpReporter) divider() {
	eprintln('> ${@METHOD}')
}

pub fn (r DumpReporter) list_of_failed_commands(failed_cmds []string) {
	eprintln('> ${@METHOD} | failed_cmds: ${failed_cmds}')
}
