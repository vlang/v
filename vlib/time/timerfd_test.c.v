import time

fn test_timerfd_periodic_expirations_are_counted() {
	$if linux {
		tfd := C.timerfd_create(C.CLOCK_MONOTONIC, C.TFD_CLOEXEC | C.TFD_NONBLOCK)
		assert tfd >= 0
		spec := C.itimerspec{
			it_value:    C.timespec{0, 5_000_000} // first expiry after 5 ms
			it_interval: C.timespec{0, 5_000_000} // then every 5 ms
		}
		assert C.timerfd_settime(tfd, 0, &spec, unsafe { nil }) == 0
		mut expirations := u64(0)
		// non-blocking fd, nothing expired yet: read fails with EAGAIN
		assert C.read(tfd, &expirations, 8) == -1
		time.sleep(23 * time.millisecond)
		assert C.read(tfd, &expirations, 8) == 8
		assert expirations >= 2 // ~4 expected; missed ticks are counted, not lost
		// disarm: zero it_value stops the timer
		zero := C.itimerspec{}
		assert C.timerfd_settime(tfd, 0, &zero, unsafe { nil }) == 0
		mut curr := C.itimerspec{}
		assert C.timerfd_gettime(tfd, &curr) == 0
		assert curr.it_value.tv_sec == 0 && curr.it_value.tv_nsec == 0
		C.close(tfd)
	}
}
