module log

// RecordingWriter implements io.Writer inside this module so the interface
// method table is generated locally.
struct RecordingWriter {
mut:
	lines []string
}

fn (mut w RecordingWriter) write(buf []u8) !int {
	w.lines << buf.bytestr()
	return buf.len
}

// `use_stdout()` builds its `ThreadSafeLog` from the struct default rather than
// through `new_thread_safe_log()`, so it does not read `-d log_default_level`
// and its logger starts at `.debug` where `new_thread_safe_log()` starts at
// `.info`.
const use_stdout_level = Level.debug

fn messages_that_passed(mut l Log) []string {
	mut w := RecordingWriter{}
	l.set_output_stream(&w)
	l.debug('m-debug')
	l.info('m-info')
	l.warn('m-warn')
	l.error('m-error')
	mut passed := []string{}
	for name in ['m-debug', 'm-info', 'm-warn', 'm-error'] {
		for line in w.lines {
			if line.contains(name) {
				passed << name
				break
			}
		}
	}
	return passed
}

fn restore_default_logger() {
	set_logger(new_thread_safe_log())
}

fn test_log_get_level_starts_at_debug() {
	mut l := Log{}
	assert l.get_level() == .debug
}

fn test_log_set_level_changes_get_level() {
	mut l := Log{}
	for level in [Level.disabled, .fatal, .error, .warn, .info, .debug] {
		l.set_level(level)
		assert l.get_level() == level
	}
}

fn test_log_level_filter_passes_the_requested_level_and_above() {
	mut l := Log{
		level: .debug
	}
	assert messages_that_passed(mut l) == ['m-debug', 'm-info', 'm-warn', 'm-error']
	l.set_level(.info)
	assert messages_that_passed(mut l) == ['m-info', 'm-warn', 'm-error']
	l.set_level(.warn)
	assert messages_that_passed(mut l) == ['m-warn', 'm-error']
	l.set_level(.error)
	assert messages_that_passed(mut l) == ['m-error']
	l.set_level(.fatal)
	assert messages_that_passed(mut l) == []string{}
	l.set_level(.disabled)
	assert messages_that_passed(mut l) == []string{}
}

fn test_default_get_level_reads_the_default_logger() {
	assert get_level() == .info
	set_level(.warn)
	assert get_level() == .warn
	set_level(.debug)
	assert get_level() == .debug
	set_level(.info)
	assert get_level() == .info
}

fn test_set_logger_swaps_the_default_logger() {
	mut custom := &ThreadSafeLog{}
	custom.set_level(.error)
	set_logger(custom)
	assert get_level() == .error
	assert unsafe { get_logger().get_level() } == .error
	// The module-level helpers follow the default logger.
	set_level(.warn)
	assert unsafe { get_logger().get_level() } == .warn
}

fn test_use_stdout_installs_a_fresh_usable_default_logger() {
	use_stdout()
	assert get_level() == use_stdout_level
	set_level(.warn)
	assert get_level() == .warn
	set_level(use_stdout_level)
	assert get_level() == use_stdout_level
}
