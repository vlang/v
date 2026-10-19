struct SpawnLogger {
mut:
	level int = 4
	lines []string
}

fn (mut logger SpawnLogger) info(message string) {
	logger.lines << message
}

fn log_from_thread(mut logger &SpawnLogger) {
	logger.info('hello from thread')
}

fn replace_from_thread(mut logger &SpawnLogger) {
	logger = &SpawnLogger{ level: 7, lines: ['replacement'] }
}

fn forward_spawn(mut logger &SpawnLogger) {
	task := spawn log_from_thread(mut logger)
	task.wait()
}

fn test_spawn_mutable_pointer_argument_preserves_slot() {
	mut logger := &SpawnLogger{}
	task := spawn log_from_thread(mut logger)
	task.wait()
	assert logger.level == 4
	assert logger.lines == ['hello from thread']
	forward_spawn(mut logger)
	assert logger.lines == ['hello from thread', 'hello from thread']
	replacement := spawn replace_from_thread(mut logger)
	replacement.wait()
	assert logger.level == 7
	assert logger.lines == ['replacement']
}
