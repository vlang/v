import os
import time
import v.ast
import v.parser
import v.pref
import v.util

fn run_true_exit_code() int {
	return os.execute('true').exit_code
}

fn spawn_execute_elapsed_ms() (i64, int) {
	sw := time.new_stopwatch()
	t := spawn run_true_exit_code()
	exit_code := t.wait()
	return sw.elapsed().milliseconds(), exit_code
}

fn test_parse_file_does_not_poison_spawn_execute_on_macos() {
	$if !macos {
		return
	}
	util.timing_set_should_print(false)
	before_ms, before_exit_code := spawn_execute_elapsed_ms()
	assert before_exit_code == 0
	mut table := ast.new_table()
	prefs := pref.new_preferences()
	_ := parser.parse_file(@FILE, mut table, .skip_comments, prefs)
	after_ms, after_exit_code := spawn_execute_elapsed_ms()
	assert after_exit_code == 0
	assert after_ms < 5000, 'spawn + os.execute slowed down from ${before_ms}ms to ${after_ms}ms'
}
