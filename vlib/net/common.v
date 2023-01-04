module net

import time

// no_deadline should be given to functions when no deadline is wanted (i.e. all functions
// return instantly)
const no_deadline = time.Time{
	unix: 0
}

// no_timeout should be given to functions when no timeout is wanted (i.e. all functions
// return instantly)
pub const no_timeout = time.Duration(0)

// infinite_timeout should be given to functions when an infinite_timeout is wanted (i.e. functions
// only ever return with data)
pub const infinite_timeout = time.infinite

// Shutdown shutsdown a socket and closes it
fn shutdown(handle int) ! {
	$if windows {
		C.shutdown(handle, C.SD_BOTH)
		socket_error(C.closesocket(handle))!
	} $else {
		C.shutdown(handle, C.SHUT_RDWR)
		socket_error(C.close(handle))!
	}
}

// Select waits for an io operation (specified by parameter `test`) to be available
fn @select(handle int, test Select, timeout time.Duration) !bool {
	set := C.fd_set{}

	C.FD_ZERO(&set)
	C.FD_SET(handle, &set)

	seconds := timeout / time.second
	microseconds := time.Duration(timeout - (seconds * time.second)).microseconds()

	mut tt := C.timeval{
		tv_sec: u64(seconds)
		tv_usec: u64(microseconds)
	}

	mut timeval_timeout := &tt

	// infinite timeout is signaled by passing null as the timeout to
	// select
	if timeout == net.infinite_timeout {
		timeval_timeout = &C.timeval(0)
	}

	match test {
		.read {
			socket_error(C.@select(handle + 1, &set, C.NULL, C.NULL, timeval_timeout))!
		}
		.write {
			socket_error(C.@select(handle + 1, C.NULL, &set, C.NULL, timeval_timeout))!
		}
		.except {
			socket_error(C.@select(handle + 1, C.NULL, C.NULL, &set, timeval_timeout))!
		}
	}

	return C.FD_ISSET(handle, &set)
}

[inline]
fn select_deadline(handle int, test Select, deadline time.Time) !bool {
	// if we have a 0 deadline here then the timeout that was passed was infinite...
	infinite := deadline.unix_time() == 0
	for infinite || time.now() <= deadline {
		timeout := if infinite { net.infinite_timeout } else { deadline - time.now() }
		ready := @select(handle, test, timeout) or {
			if err.code() == 4 {
				// Spurious wakeup from signal, keep waiting
				continue
			}

			// NOT a spurious wakeup
			return err
		}

		return ready
	}

	// Deadline elapsed
	return false
}

// wait_for_common wraps the common wait code
fn wait_for_common(handle int, deadline time.Time, timeout time.Duration, test Select) ! {
	// Convert timeouts to deadlines
	real_deadline := if timeout == net.infinite_timeout {
		time.unix(0)
	} else if timeout == 0 {
		// No timeout set, so assume deadline
		deadline
	} else if timeout < 0 {
		// TODO(emily): Do something nicer here :)
		panic('invalid negative timeout')
	} else {
		// timeout
		time.now().add(timeout)
	}

	ready := select_deadline(handle, test, real_deadline)!

	if ready {
		return
	}

	return err_timed_out
}

// wait_for_write waits for a write io operation to be available
fn wait_for_write(handle int, deadline time.Time, timeout time.Duration) ! {
	return wait_for_common(handle, deadline, timeout, .write)
}

// wait_for_read waits for a read io operation to be available
fn wait_for_read(handle int, deadline time.Time, timeout time.Duration) ! {
	return wait_for_common(handle, deadline, timeout, .read)
}
