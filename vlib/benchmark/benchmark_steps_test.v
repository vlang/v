module benchmark

import term
import time

fn test_new_benchmark_defaults_to_verbose_and_zero_counts() {
	mut b := new_benchmark()
	assert b.verbose == true
	assert b.no_cstep == false
	assert b.ntotal == 0
	assert b.nok == 0
	assert b.nfail == 0
	assert b.nskip == 0
	assert b.nexpected_steps == 0
	assert b.cstep == 0
	assert b.total_duration() >= 0
}

fn test_new_benchmark_no_cstep_skips_step_counting() {
	mut b := new_benchmark_no_cstep()
	assert b.no_cstep == true
	mut started := new_benchmark()
	started.step()
	assert started.cstep == 1
}

fn test_new_benchmark_pointer_is_a_heap_instance_with_the_same_defaults() {
	mut b := &Benchmark{}
	assert b.verbose == false
	assert b.nok == 0
	assert b.nfail == 0
	assert b.nskip == 0
	assert b.no_cstep == false
}

fn test_step_counts_steps_only_when_cstep_is_enabled() {
	mut counting := new_benchmark()
	counting.step()
	counting.step()
	counting.step()
	assert counting.cstep == 3

	mut not_counting := new_benchmark_no_cstep()
	not_counting.step()
	not_counting.step()
	not_counting.step()
	assert not_counting.cstep == 0
}

fn test_step_restart_leaves_the_step_count_alone() {
	mut b := new_benchmark()
	b.step()
	b.step()
	assert b.cstep == 2
	b.step_restart()
	assert b.cstep == 2
}

fn test_ok_fail_and_skip_update_the_totals() {
	mut b := new_benchmark()
	b.ok()
	b.ok()
	b.fail()
	b.skip()
	assert b.nok == 2
	assert b.nfail == 1
	assert b.nskip == 1
	assert b.ntotal == 4
}

fn test_fail_many_and_ok_many_batch_the_totals() {
	mut b := new_benchmark()
	b.fail_many(3)
	b.ok_many(5)
	assert b.nfail == 3
	assert b.nok == 5
	assert b.ntotal == 8
}

fn test_neither_fail_nor_ok_touches_nothing() {
	mut b := new_benchmark()
	b.neither_fail_nor_ok()
	assert b.nok == 0
	assert b.nfail == 0
	assert b.nskip == 0
	assert b.ntotal == 0
}

fn test_stop_freezes_the_benchmark_timer() {
	mut b := new_benchmark()
	time.sleep(20 * time.millisecond)
	b.stop()
	first := b.total_duration()
	time.sleep(20 * time.millisecond)
	assert b.total_duration() == first
}

fn test_set_total_expected_steps_adds_the_progress_label() {
	mut b := new_benchmark()
	b.set_total_expected_steps(3)
	b.step()
	message := b.step_message_ok('working')
	assert term.strip_ansi(message).contains('OK')
	assert message.contains('[1/3]')
	b.step()
	next := b.step_message_ok('working')
	assert next.contains('[2/3]')
}

fn test_no_cstep_progress_label_shows_tmp_instead_of_the_counter() {
	mut b := new_benchmark_no_cstep()
	b.set_total_expected_steps(3)
	b.step()
	message := b.step_message_ok('working')
	assert term.strip_ansi(message).contains('OK')
	assert message.contains('TMP1/3')
}

fn test_total_message_summarises_the_counted_steps() {
	mut b := new_benchmark()
	b.ok()
	b.ok()
	b.fail()
	b.skip()
	message := term.strip_ansi(b.total_message('summary run'))
	assert message.contains('2 passed')
	assert message.contains('1 failed')
	assert message.contains('1 skipped')
	assert message.contains('4 total.')
	assert message.contains('Elapsed time:')
	assert message.contains('Summary for summary run:')
}

fn test_step_message_fail_and_skip_use_their_own_labels() {
	mut b := new_benchmark()
	fail_message := term.strip_ansi(b.step_message_fail('bad'))
	skip_message := term.strip_ansi(b.step_message_skip('skipped'))
	assert fail_message.contains('FAIL')
	assert fail_message.contains('bad')
	assert skip_message.contains('SKIP')
	assert skip_message.contains('skipped')
}
