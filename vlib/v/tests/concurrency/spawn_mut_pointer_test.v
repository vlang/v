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

struct SpawnLoggerHolder {
mut:
	logger &SpawnLogger = unsafe { nil }
}

fn next_spawn_pointer_slot(mut calls []int) int {
	calls[0]++
	return 0
}

fn read_spawn_pointer(logger &SpawnLogger) int {
	return logger.level
}

fn test_spawn_mutable_pointer_lvalue_arguments() {
	mut calls := [0]
	mut loggers := [&SpawnLogger{}]
	indexed := spawn replace_from_thread(mut loggers[next_spawn_pointer_slot(mut calls)])
	indexed.wait()
	assert calls == [1]
	assert loggers[0].level == 7
	assert loggers[0].lines == ['replacement']

	mut fixed := [&SpawnLogger{}]!
	fixed_indexed := spawn replace_from_thread(mut fixed[next_spawn_pointer_slot(mut calls)])
	fixed_indexed.wait()
	assert calls == [2]
	assert fixed[0].level == 7

	mut array_pointer := &loggers
	pointed := spawn log_from_thread(mut (*array_pointer)[next_spawn_pointer_slot(mut calls)])
	pointed.wait()
	assert calls == [3]
	assert loggers[0].lines == ['replacement', 'hello from thread']

	parenthesized := spawn replace_from_thread(mut (loggers[next_spawn_pointer_slot(mut calls)]))
	parenthesized.wait()
	assert calls == [4]
	assert loggers[0].lines == ['replacement']

	log_from_thread(mut loggers[next_spawn_pointer_slot(mut calls)])
	assert calls == [5]
	assert loggers[0].lines == ['replacement', 'hello from thread']

	gated := spawn replace_from_thread(mut loggers#[next_spawn_pointer_slot(mut calls)])
	gated.wait()
	assert calls == [6]
	assert loggers[0].lines == ['replacement']
	log_from_thread(mut loggers#[next_spawn_pointer_slot(mut calls)])
	assert calls == [7]
	assert loggers[0].lines == ['replacement', 'hello from thread']

	mut holder := SpawnLoggerHolder{ logger: &SpawnLogger{} }
	callback := replace_from_thread
	field := spawn callback(mut holder.logger)
	field.wait()
	assert holder.logger.level == 7
	reader := spawn read_spawn_pointer(holder.logger)
	assert reader.wait() == 7
}

struct SpawnEmbeddedLoggerHolder {
	SpawnLoggerHolder
}

struct SpawnOtherLoggerHolder {
	value int
}

type SpawnAnyLoggerHolder = SpawnLoggerHolder | SpawnOtherLoggerHolder

interface SpawnHasLogger {
mut:
	logger &SpawnLogger
}

fn test_mutable_pointer_dereferenced_slot() {
	mut logger := &SpawnLogger{}
	mut slot := &logger
	// Explicit pointer dereferencing requires unsafe; the slot remains alive until join.
	unsafe {
		replace_from_thread(mut *slot)
	}
	assert logger.level == 7
	logger = &SpawnLogger{}
	unsafe {
		task := spawn replace_from_thread(mut *slot)
		task.wait()
	}
	assert logger.level == 7
}

fn test_mutable_pointer_embedded_field() {
	mut holder := SpawnEmbeddedLoggerHolder{ SpawnLoggerHolder: SpawnLoggerHolder{ logger: &SpawnLogger{} } }
	replace_from_thread(mut holder.logger)
	assert holder.logger.level == 7
	holder.logger = &SpawnLogger{}
	task := spawn replace_from_thread(mut holder.logger)
	task.wait()
	assert holder.logger.level == 7
}

fn test_mutable_pointer_smartcast_field() {
	mut holder := SpawnAnyLoggerHolder(SpawnLoggerHolder{ logger: &SpawnLogger{} })
	if mut holder is SpawnLoggerHolder {
		replace_from_thread(mut holder.logger)
		assert holder.logger.level == 7
		holder.logger = &SpawnLogger{}
		task := spawn replace_from_thread(mut holder.logger)
		task.wait()
		assert holder.logger.level == 7
	} else {
		assert false
	}
}

fn test_mutable_pointer_interface_field() {
	mut holder := SpawnHasLogger(SpawnLoggerHolder{ logger: &SpawnLogger{} })
	replace_from_thread(mut holder.logger)
	assert holder.logger.level == 7
	holder.logger = &SpawnLogger{}
	task := spawn replace_from_thread(mut holder.logger)
	task.wait()
	assert holder.logger.level == 7
}
