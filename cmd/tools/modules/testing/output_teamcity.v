module testing

// TeamcityReporter implements the interface `testing.Reporter`.
// It is used by `v -test-runner teamcity test .`
pub struct TeamcityReporter {
}

pub fn (r TeamcityReporter) session_start(message string, mut ts TestSession) {
}

pub fn (r TeamcityReporter) session_stop(message string, mut ts TestSession) {
}

pub fn (r TeamcityReporter) report(index int, message LogMessage) {
	name := r.get_test_suite_name_by_file(message.file)
	match message.kind {
		.cmd_begin {
			eprintln("##teamcity[testSuiteStarted name='${name}' flowId='${message.flow_id}']")
		}
		.cmd_end {
			eprintln("##teamcity[testSuiteFinished name='${name}' flowId='${message.flow_id}' duration='${message.took}']")
		}
		.cannot_compile {
			eprintln("##teamcity[testFailed name='${name}' message='${message.message}']")
		}
		else {}
	}
}

pub fn (r TeamcityReporter) get_test_suite_name_by_file(path string) string {
	file_name := path.replace('\\', '/').split('/').last()
	return file_name.split('.').first()
}

pub fn (r TeamcityReporter) report_stop() {
}

pub fn (r TeamcityReporter) progress(index int, message string) {
}

pub fn (r TeamcityReporter) update_last_line(index int, message string) {
}

pub fn (r TeamcityReporter) update_last_line_and_move_to_next(index int, message string) {
}

pub fn (r TeamcityReporter) message(index int, message string) {
}

pub fn (r TeamcityReporter) divider() {
}

pub fn (r TeamcityReporter) worker_threads_start(files []string, mut ts TestSession) {
}

pub fn (r TeamcityReporter) worker_threads_finish(mut ts TestSession) {
}

pub fn (r TeamcityReporter) list_of_failed_commands(failed_cmds []string) {
}
