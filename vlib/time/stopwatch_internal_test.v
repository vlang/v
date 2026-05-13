module time

fn test_stopwatch_elapsed_uses_explicit_started_state() {
	sw := StopWatch{
		started: true
		start:   0
		end:     7
	}
	assert sw.elapsed() == 7
}
