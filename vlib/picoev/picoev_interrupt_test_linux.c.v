module picoev

import os

#include <unistd.h>

fn C.alarm(seconds u32) u32

fn ignore_test_signal(_ os.Signal) {}

fn test_loop_once_ignores_eintr() {
	mut pv := &Picoev{}
	pv.loop = create_epoll_loop(0) or { panic(err) }
	pv.init()
	prev_handler := os.signal_opt(.alrm, ignore_test_signal) or { panic(err) }
	defer {
		C.alarm(0)
		os.signal_opt(.alrm, prev_handler) or { panic(err) }
	}
	C.alarm(1)
	assert pv.loop_once(2) == 0
}
