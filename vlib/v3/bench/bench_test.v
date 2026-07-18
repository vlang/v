module bench

fn test_memory_limit_error_starts_at_limit() {
	assert memory_limit_error(default_memory_limit_kb - 1, default_memory_limit_kb, 'parse') == ''

	message := memory_limit_error(default_memory_limit_kb, default_memory_limit_kb, 'parse')
	assert message.contains('10240 MiB RSS after parse')
	assert message.contains('limit: 10 GiB')
	assert message.contains('`-no-memory-limit`')
}

fn test_disable_memory_limit() {
	mut b := new()
	assert memory_limit_error(default_memory_limit_kb, b.memory_limit_kb, 'check') != ''
	b.disable_memory_limit()
	assert memory_limit_error(default_memory_limit_kb, b.memory_limit_kb, 'check') == ''
}
