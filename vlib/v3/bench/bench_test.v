module bench

import os

const memory_monitor_test_child = 'V3_MEMORY_MONITOR_TEST_CHILD'

fn test_memory_limit_error_starts_at_limit() {
	assert memory_limit_error(default_memory_limit_kb - 1, default_memory_limit_kb, 'after parse',
		'RSS') == ''

	message := memory_limit_error(default_memory_limit_kb, default_memory_limit_kb, 'after parse',
		'RSS')
	assert message.contains('10240 MiB RSS after parse')
	assert message.contains('limit: 10 GiB')
	assert message.contains('`-no-memory-limit`')
}

fn test_disable_memory_limit() {
	mut b := new()
	assert memory_limit_error(default_memory_limit_kb, b.memory_limit_kb, 'after check', 'RSS') != ''
	b.disable_memory_limit()
	assert memory_limit_error(default_memory_limit_kb, b.memory_limit_kb, 'after check', 'RSS') == ''
}

fn test_limit_memory_metric_is_available() {
	memory := current_limit_memory()
	assert memory.kb > 0
	$if macos {
		assert memory.metric == 'physical footprint'
	} $else {
		assert memory.metric == 'RSS'
	}
}

fn test_physical_footprint_suffix_only_prints_physical_footprint() {
	assert physical_footprint_suffix(LimitMemory{
		kb:     2 * 1024
		metric: 'physical footprint'
	}).contains('2 MB physical footprint')
	assert physical_footprint_suffix(LimitMemory{
		kb:     2 * 1024
		metric: 'RSS'
	}) == ''
}

fn test_memory_monitor_exits_above_limit() {
	if os.getenv(memory_monitor_test_child) == '1' {
		monitor_memory_limit(1)
		assert false
		return
	}
	mut child := os.new_process(os.executable())
	mut environment := os.environ()
	environment[memory_monitor_test_child] = '1'
	child.set_environment(environment)
	child.set_redirect_stdio()
	child.wait()
	error_output := child.stderr_slurp()
	child.close()
	assert child.code == 1
	assert error_output.contains('during compilation'), error_output
	assert error_output.contains('limit: 0 GiB'), error_output
}
