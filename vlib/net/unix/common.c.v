module unix

import time
import net

// no_deadline disables the absolute deadline; the configured socket timeout still applies.
const no_deadline = time.unix(0)
// no_timeout disables the relative socket timeout. An explicit deadline still applies;
// with no deadline, socket waits block until ready or an error occurs.
const no_timeout = time.Duration(0)
// infinite_timeout disables the relative socket timeout, like no_timeout.
// An explicit deadline still applies.
const infinite_timeout = time.infinite

fn C.strncpy(&char, &char, i32)

// close a socket, given its file descriptor `handle`.
pub fn close(handle int) ! {
	net.close(handle)!
}

// shutdown shutsdown a socket, given its file descriptor `handle`.
// By default it shuts it down in both directions, both for reading
// and for writing. You can change that using `net.shutdown(handle, how: .read)`
// or `net.shutdown(handle, how: .write)`
pub fn shutdown(handle int, config net.ShutdownConfig) int {
	return net.shutdown(handle, config)
}

// Select waits for an io operation (specified by parameter `test`) to be available
fn select(handle int, test Select, timeout time.Duration) !bool {
	set := C.fd_set{}

	C.FD_ZERO(&set)
	C.FD_SET(handle, &set)

	mut tt := C.timeval{}
	mut timeval_timeout := &tt

	// infinite timeout is signaled by passing null as the timeout to
	// select.
	if timeout == infinite_timeout {
		timeval_timeout = &C.timeval(unsafe { nil })
	} else {
		seconds := timeout / time.second
		microseconds := time.Duration(timeout - (seconds * time.second)).microseconds()
		tt = C.timeval{
			tv_sec:  u64(seconds)
			tv_usec: u64(microseconds)
		}
	}

	match test {
		.read {
			net.socket_error(C.select(handle + 1, &set, unsafe { nil }, unsafe { nil }, timeval_timeout))!
		}
		.write {
			net.socket_error(C.select(handle + 1, unsafe { nil }, &set, unsafe { nil }, timeval_timeout))!
		}
		.except {
			net.socket_error(C.select(handle + 1, unsafe { nil }, unsafe { nil }, &set, timeval_timeout))!
		}
	}

	return C.FD_ISSET(handle, &set) != 0
}

@[inline]
fn select_deadline(handle int, test Select, deadline time.Time) !bool {
	return select_deadline_chunked(handle, test, deadline, 24 * time.hour)
}

fn select_deadline_chunked(handle int, test Select, deadline time.Time, max_wait time.Duration) !bool {
	// Only the unset deadline sentinels mean an infinite wait.
	infinite := !deadline_set(deadline)
	for {
		now := time.now()
		if !infinite && now > deadline {
			break
		}
		// Clamp before subtracting: distant dates may not fit in a Duration.
		chunked := !infinite && deadline > now.add(max_wait)
		timeout := if infinite {
			net.infinite_timeout
		} else if chunked {
			max_wait
		} else {
			deadline - now
		}
		ready := select(handle, test, timeout) or {
			if err.code() == 4 {
				// Spurious wakeup from signal, keep waiting
				continue
			}

			// NOT a spurious wakeup
			return err
		}

		if !ready && chunked {
			continue
		}
		return ready
	}

	// Deadline elapsed
	return error_with_code(net.err_timed_out.msg(), net.err_timed_out.code())
}

// deadline_set is whether a deadline was set: anything but the zero `time.Time{}` a connection
// starts with and the `time.unix(0)` that select_deadline reads as infinite. A deadline already in
// the past, even before 1970, is set: it is due.
fn deadline_set(deadline time.Time) bool {
	return !deadline.is_zero() && deadline != time.unix(0)
}

// wait_for_common wraps the common wait code
fn wait_for_common(handle int, deadline time.Time, timeout time.Duration, test Select) ! {
	// Convert timeouts to deadlines. A deadline the caller set wins over the timeout, as in net.
	real_deadline := if deadline_set(deadline) {
		deadline
	} else if timeout == net.infinite_timeout {
		time.unix(0)
	} else if timeout <= 0 {
		// No timeout set, so assume deadline
		deadline
	} else {
		// timeout
		time.now().add(timeout)
	}

	ready := select_deadline(handle, test, real_deadline)!

	if ready {
		return
	}

	return error_with_code(net.err_timed_out.msg(), net.err_timed_out.code())
}

// wait_for_write waits for a write io operation to be available
fn wait_for_write(handle int, deadline time.Time, timeout time.Duration) ! {
	return wait_for_common(handle, deadline, timeout, .write)
}

// wait_for_read waits for a read io operation to be available
fn wait_for_read(handle int, deadline time.Time, timeout time.Duration) ! {
	return wait_for_common(handle, deadline, timeout, .read)
}

@[inline]
fn wrap_read_result(result int) !int {
	if result != 0 {
		return result
	}
	return error('none')
}
