import time
import benchmark
import term

fn set_step_elapsed(mut b benchmark.Benchmark, elapsed time.Duration) {
	// Preserve the known duration when recording the step stops its timer.
	b.step_timer.end = b.step_timer.start + u64(elapsed.nanoseconds())
	b.step_timer.pause()
}

fn test_measure() {
	mut b := benchmark.start()
	set_step_elapsed(mut b, 200 * time.millisecond)
	assert b.measure('work') == 200_000
}

fn test_record_measure() {
	mut b := benchmark.start()
	set_step_elapsed(mut b, 100 * time.millisecond)
	assert b.record_measure('step 1') == 100_000
	assert b.step_timer.end == 0

	set_step_elapsed(mut b, 150 * time.millisecond)
	assert b.record_measure('step 2') == 150_000

	res := b.all_recorded_measures()
	assert res.contains('100.000 ms in step 1')
	assert res.contains('150.000 ms in step 2')
	assert res.contains('SPENT')
}

fn test_total_message() {
	mut b := benchmark.start()
	for ms in 1 .. 101 {
		set_step_elapsed(mut b, ms * time.millisecond)
		assert b.record_measure('repeated') == ms * 1_000
	}

	res := b.total_message('repeated')
	assert res.contains(' Min: 1.000 ms.')
	assert res.contains(' Max: 100.000 ms.')
	assert res.contains(' Avg: 50.500 ms')

	set_step_elapsed(mut b, time.millisecond)
	assert b.record_measure('single') == 1_000
	res2 := b.total_message('single')

	assert !res2.contains(' Min: ')
	assert !res2.contains(' Max: ')
	assert !res2.contains(' Avg: ')
}

fn test_step_message_handles_ansi_label_longer_than_label_width() {
	mut b := benchmark.start()
	ansi_fail_label := '\x1b[41m\x1b[1m\x1b[37m FAIL \x1b[39m\x1b[22m\x1b[49m'
	long_cmd := 'command: /nix/store/' + 'x'.repeat(90) + '-vlang/build/source/v -o v_g -g cmd/v'
	message := b.step_message_with_label(ansi_fail_label, long_cmd)

	assert term.strip_ansi(message).contains('FAIL')
	assert message.contains(long_cmd)
}
