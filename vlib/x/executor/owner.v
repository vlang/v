module executor

import sync

// is_owner_thread reports whether the caller is currently inside an active owner pump.
pub fn (e &Executor) is_owner_thread() bool {
	e.mutex.lock()
	is_owner := e.owner_active && e.owner_thread_id == sync.thread_id()
	e.mutex.unlock()
	return is_owner
}

// assert_owner_thread returns an error when called outside the active owner pump.
pub fn (e &Executor) assert_owner_thread() ! {
	if !e.is_owner_thread() {
		return error(err_not_owner_thread)
	}
}

fn (mut e Executor) begin_owner_pump(long_running bool) ! {
	e.mutex.lock()
	if e.stopped && long_running {
		e.mutex.unlock()
		return error(err_executor_stopped)
	}
	if e.pumping {
		e.mutex.unlock()
		return error(err_owner_pump_running)
	}
	e.pumping = true
	e.run_active = long_running
	e.owner_active = true
	e.owner_thread_id = sync.thread_id()
	e.mutex.unlock()
}

fn (mut e Executor) end_owner_pump() {
	e.mutex.lock()
	e.pumping = false
	e.run_active = false
	e.owner_active = false
	e.owner_thread_id = 0
	e.mutex.unlock()
}
