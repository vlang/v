module executor

fn test_new_rejects_zero_queue_size() {
	new(queue_size: 0) or {
		assert err.msg() == 'executor: queue size must be positive'
		return
	}
	assert false, 'new accepted a zero queue size'
}

fn test_new_rejects_negative_queue_size() {
	new(queue_size: -1) or {
		assert err.msg() == 'executor: queue size must be positive'
		return
	}
	assert false, 'new accepted a negative queue size'
}

fn test_try_post_rejects_nil_job() {
	mut ex := new(queue_size: 1)!
	nil_job := unsafe { JobFn(nil) }
	ex.try_post(nil_job) or {
		assert err.msg() == 'executor: job function is nil'
		ex.stop()
		return
	}
	assert false, 'try_post accepted a nil job'
}
