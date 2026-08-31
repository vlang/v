module executor

import context
import sync
import time

@[heap]
struct AdmissionProbeContext {
mut:
	done_ch                chan int
	entered_admission_wait chan bool
	mutex                  &sync.Mutex = sync.new_mutex()
	done_requested         bool
	signaled               bool
}

fn new_admission_probe_context() &AdmissionProbeContext {
	return &AdmissionProbeContext{
		done_ch:                chan int{}
		entered_admission_wait: chan bool{cap: 1}
		mutex:                  sync.new_mutex()
	}
}

fn (ctx &AdmissionProbeContext) deadline() ?time.Time {
	return none
}

fn (ctx &AdmissionProbeContext) value(key context.Key) ?context.Any {
	_ = key
	return none
}

fn (mut ctx AdmissionProbeContext) done() chan int {
	ctx.mutex.lock()
	ctx.done_requested = true
	done := ctx.done_ch
	ctx.mutex.unlock()
	return done
}

fn (mut ctx AdmissionProbeContext) err() IError {
	ctx.mutex.lock()
	if ctx.done_requested && !ctx.signaled {
		ctx.signaled = true
		ctx.entered_admission_wait <- true
	}
	ctx.mutex.unlock()
	return none
}

fn test_many_producers_submit_bounded_work() {
	queue_size := 4
	mut producer_count := 8
	$if windows && (gcc || msvc) {
		producer_count = queue_size + 1
	}
	mut ex := new(queue_size: queue_size)!
	attempting := chan bool{cap: producer_count}
	accepted := chan bool{cap: producer_count}
	ran := chan int{cap: producer_count}
	mut producers := []thread{}

	for i in 0 .. producer_count {
		producers << spawn fn [mut ex, attempting, accepted, ran, i] () {
			attempting <- true
			ex.post_with_context(context.background(), fn [ran, i] () ! {
				ran <- i
			}) or {
				accepted <- false
				return
			}
			accepted <- true
		}()
	}

	// Wait for a small proof of concurrent admission pressure, not every producer
	// thread. The extra attempt should be blocked until the owner drains capacity.
	for _ in 0 .. queue_size + 1 {
		assert wait_for_bool(attempting, 'producer did not reach admission')
	}
	for _ in 0 .. queue_size {
		assert wait_for_bool(accepted, 'producer did not finish admission')
	}
	assert_no_bool(accepted, 'producer was admitted before capacity opened')
	assert ex.drain_pending(queue_size)! == queue_size

	mut total := 0
	for _ in 0 .. queue_size {
		total += wait_for_int(ran, 'producer job did not run')
	}

	for _ in 0 .. producer_count - queue_size {
		assert wait_for_bool(accepted, 'producer did not finish admission after capacity opened')
	}
	assert ex.drain_pending(producer_count)! == producer_count - queue_size
	for _ in 0 .. producer_count - queue_size {
		total += wait_for_int(ran, 'producer job did not run')
	}
	expected_total := ((producer_count - 1) * producer_count) / 2
	assert total == expected_total

	for producer in producers {
		producer.wait()
	}
	ex.stop()
	assert !ex.run_one()!
	ex.wait()!
}

fn test_stop_wakes_waiting_submitter_without_accepting_job() {
	mut ex := new(queue_size: 1)!
	probe := new_admission_probe_context()
	result := chan string{cap: 1}
	ran := chan bool{cap: 1}

	ex.try_post(fn () ! {})!
	submitter := spawn fn [mut ex, probe, result, ran] () {
		mut submit_ctx := context.Context(probe)
		ex.post_with_context(submit_ctx, fn [ran] () ! {
			ran <- true
		}) or {
			result <- err.msg()
			return
		}
		result <- 'accepted'
	}()

	assert wait_for_bool(probe.entered_admission_wait, 'submitter did not enter admission wait')
	ex.stop()
	msg := wait_for_string(result, 'waiting submitter was not woken by stop')
	assert msg == 'executor: executor is closed'
	assert_no_bool(ran, 'job was accepted after stop')
	submitter.wait()
}

fn wait_for_bool(signal chan bool, message string) bool {
	timeout := executor_stress_signal_timeout()
	select {
		value := <-signal {
			return value
		}
		timeout {
			assert false, message
		}
	}
	return false
}

fn wait_for_int(signal chan int, message string) int {
	timeout := executor_stress_signal_timeout()
	select {
		value := <-signal {
			return value
		}
		timeout {
			assert false, message
		}
	}
	return 0
}

fn wait_for_string(signal chan string, message string) string {
	timeout := executor_stress_signal_timeout()
	select {
		value := <-signal {
			return value
		}
		timeout {
			assert false, message
		}
	}
	return ''
}

fn executor_stress_signal_timeout() time.Duration {
	$if windows && gcc {
		return 15 * time.second
	}
	$if windows {
		return 5 * time.second
	}
	return 1 * time.second
}

fn assert_no_bool(signal chan bool, message string) {
	select {
		_ := <-signal {
			assert false, message
		}
		50 * time.millisecond {}
	}
}
