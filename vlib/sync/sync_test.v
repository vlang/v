module sync

import time

fn f(mut m Mutex, mut l &bool) {
	m.m_lock()
	unsafe {
		(*l) = false
	}
}

fn test_mutex_blocking() {
	mut m1 := new_mutex()
	mut l := true
	m1.m_lock()
	$if windows {
		assert m1.state == .waiting
	}
	unsafe {
		go f(mut m1, mut &l)
	}
	time.sleep_ms(10)
	assert l == true
}

fn test_mutex_try_lock() {
	mut m1 := new_mutex()
	assert m1.try_lock() == true
	mut sw := time.new_stopwatch({})
	assert m1.try_lock() == false
	assert sw.elapsed().milliseconds() < 10
}
