module testing

fn test_normal_reporter_keeps_active_messages_through_gc() {
	mut reporter := NormalReporter{}
	for i in 0 .. 256 {
		file := 'reporter_file_${i}'
		reporter.report(i, LogMessage{
			kind: .compile_begin
			file: file
		})
		reporter.report(i, LogMessage{
			kind: .cmd_begin
			file: file
		})
		if i % 32 == 31 {
			gc_collect()
		}
	}
	gc_collect()
	assert reporter.compiling.len == 256
	assert reporter.running.len == 256
	for i in 0 .. 256 {
		file := 'reporter_file_${i}'
		assert (reporter.compiling[file] or { panic('missing compile message') }).file == file
		assert (reporter.running[file] or { panic('missing run message') }).file == file
		reporter.report(i, LogMessage{
			kind: .compile_end
			file: file
		})
		reporter.report(i, LogMessage{
			kind: .cmd_end
			file: file
		})
	}
	assert reporter.compiling.len == 0
	assert reporter.running.len == 0
	assert reporter.ctimes.len == 256
	assert reporter.rtimes.len == 256
}
