import x.executor

fn test_ierror_none_cast_in_imported_result_function_compiles() {
	mut runner := executor.new(queue_size: 1)!
	runner.stop()
	assert runner.drain_pending(1)! == 0
}
