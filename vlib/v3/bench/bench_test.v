module bench

import os
import time

const memory_monitor_test_child = 'V3_MEMORY_MONITOR_TEST_CHILD'

fn test_memory_limit_error_starts_at_limit() {
	assert memory_limit_error(default_memory_limit_kb - 1, default_memory_limit_kb, 'after parse',
		'RSS') == ''

	message := memory_limit_error(default_memory_limit_kb, default_memory_limit_kb, 'after parse',
		'RSS')
	assert message.contains('10176 MiB RSS after parse')
	assert message.contains('limit: 10176 MiB')
	assert message.contains('`-no-memory-limit`')
}

fn test_disable_memory_limit() {
	mut b := new()
	assert memory_limit_error(default_memory_limit_kb, b.memory_limit_kb, 'after check', 'RSS') != ''
	b.disable_memory_limit()
	assert memory_limit_error(default_memory_limit_kb, b.memory_limit_kb, 'after check', 'RSS') == ''
}

fn test_self_host_memory_limit() {
	mut b := new()
	b.use_self_host_memory_limit()
	assert memory_limit_error(self_host_memory_limit_kb, b.memory_limit_kb, 'after transform',
		'RSS').contains('(limit: 9984 MiB)')
}

fn test_compiler_tree_memory_limit() {
	mut b := new()
	b.use_compiler_tree_memory_limit()
	assert memory_limit_error(compiler_tree_memory_limit_kb, b.memory_limit_kb, 'after transform',
		'RSS').contains('(limit: 9984 MiB)')
}

fn test_step_parts_record_individual_timings() {
	mut b := new()
	b.disable_memory_limit()
	b.step_parts([
		StepPart{
			name:     'parse .vh'
			time_us:  1250
			parallel: true
		},
		StepPart{
			name:    'parse .v'
			time_us: 2750
		},
	])
	assert b.steps.len == 2
	assert b.steps[0].name == 'parse .vh (parallel)'
	assert b.steps[0].time_us == 1250
	assert b.steps[0].stage_peak_ram_kb == 0
	assert b.steps[1].name == 'parse .v'
	assert b.steps[1].time_us == 2750
	assert b.steps[1].stage_peak_ram_kb == 0
	b.step_measured('ownership', 500)
	assert b.steps[2].stage_peak_ram_kb == 0
}

fn test_finish_stage_memory_reports_and_resets_sampled_peak() {
	mut b := new()
	current := current_rss_kb()
	mut monitor := unsafe { &StageMemoryMonitor(voidptr(b.stage_memory)) }
	monitor.mutex.lock()
	monitor.rss_peak_kb = current + 2048
	monitor.mutex.unlock()
	assert b.finish_stage_memory(current) == current + 2048
	monitor.mutex.lock()
	reset_peak := monitor.rss_peak_kb
	monitor.mutex.unlock()
	assert reset_peak == current
}

fn test_stage_memory_monitor_stops_before_state_release() {
	mut b := new()
	b.disable_memory_limit()
	b.memory_monitor_interval = time.minute
	b.start_memory_monitor()
	stopwatch := time.new_stopwatch()
	b.stop_memory_monitor()
	assert !b.memory_monitor_started
	assert stopwatch.elapsed() < time.second
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

fn test_shorten_home_path() {
	home := os.home_dir()
	if home.len == 0 {
		return
	}
	assert shorten_home_path(home) == '~'
	assert shorten_home_path('${home}/code/project/main.v') == '~/code/project/main.v'
	assert shorten_home_path('${home}_other/code/project/main.v') == '${home}_other/code/project/main.v'
	assert shorten_home_path('/tmp/project/main.v') == '/tmp/project/main.v'
}
