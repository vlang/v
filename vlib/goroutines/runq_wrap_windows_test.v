// vtest build: windows
module goroutines

// This file starts no workers, so synthetic goroutines stay on the queues without executing.
fn test_runq_put_slow_accepts_wrapped_counters() {
	mut pp := Processor{
		runq_head: u32(0xffff_ff80)
		runq_tail: u32(0x80)
	}
	for i in 0 .. local_queue_size {
		pp.runq[(pp.runq_head + u32(i)) % local_queue_size] = &Goroutine{
			id:     u64(i)
			status: .runnable
		}
	}
	extra := &Goroutine{
		id:     u64(local_queue_size)
		status: .runnable
	}
	runq_put_slow(mut pp, extra, pp.runq_head, pp.runq_tail)
	assert pp.runq_head == 0
	assert pp.runq_tail == 0x80

	gsched.mu.acquire()
	defer {
		gsched.mu.release()
	}
	for i in 0 .. local_queue_size / 2 {
		gp := gsched.runq.pop()
		assert gp != unsafe { nil }
		assert gp.id == u64(i)
	}
	gp := gsched.runq.pop()
	assert gp == extra
	assert gsched.runq.empty()
}
