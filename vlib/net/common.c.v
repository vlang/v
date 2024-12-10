module net

import time

// no_deadline should be given to functions when no deadline is wanted (i.e. all functions
// return instantly)
const no_deadline = time.unix(0)

// no_timeout should be given to functions when no timeout is wanted (i.e. all functions
// return instantly)
pub const no_timeout = time.Duration(0)

// infinite_timeout should be given to functions when an infinite_timeout is wanted (i.e. functions
// only ever return with data)
pub const infinite_timeout = time.infinite

// ShutdownDirection is used by `net.shutdown`, for specifying the direction for which the
// communication will be cut.
pub enum ShutdownDirection {
	read
	write
	read_and_write
}

@[params]
pub struct ShutdownConfig {
pub:
	how ShutdownDirection = .read_and_write
}

// shutdown shutsdown a socket, given its file descriptor `handle`.
// By default it shuts it down in both directions, both for reading
// and for writing. You can change that using `net.shutdown(handle, how: .read)`
// or `net.shutdown(handle, how: .write)`
// In non-blocking mode, `shutdown()` may not succeed immediately,
// so `select` is also used to make sure that the function doesn't return an incorrect result.
pub fn shutdown(handle int, config ShutdownConfig) int {
	res := C.shutdown(handle, int(config.how))
	$if !net_nonblocking_sockets ? {
		return res
	} $else {
		if res == 0 {
			return 0
		}
		ecode := error_code()
		if (is_windows && ecode == int(error_ewouldblock)) || (!is_windows && res == -1
			&& ecode in [int(error_einprogress), int(error_eagain), C.EINTR]) {
			write_result := select_deadline(handle, .write, time.now().add(connect_timeout)) or {
				false
			}
			err := 0
			len := sizeof(err)
			xyz := C.getsockopt(handle, C.SOL_SOCKET, C.SO_ERROR, &err, &len)
			if xyz == 0 && err == 0 {
				return 0
			}
			if write_result {
				if xyz == 0 {
					return err
				}
				return 0
			}
		}
		return -ecode
	}
}

// close a socket, given its file descriptor `handle`.
// In non-blocking mode, if `close()` does not succeed immediately,
// it causes an error to be propagated to `TcpSocket.close()`, which is not intended.
// Therefore, `select` is used just like `connect()`.
pub fn close(handle int) ! {
	res := $if windows {
		C.closesocket(handle)
	} $else {
		C.close(handle)
	}
	$if !net_nonblocking_sockets ? {
		socket_error(res)!
		return
	} $else {
		if res == 0 {
			return
		}
		ecode := error_code()
		if (is_windows && ecode == int(error_ewouldblock)) || (!is_windows && res == -1
			&& ecode in [int(error_einprogress), int(error_eagain), C.EINTR]) {
			write_result := select_deadline(handle, .write, time.now().add(connect_timeout))!
			err := 0
			len := sizeof(err)
			xyz := C.getsockopt(handle, C.SOL_SOCKET, C.SO_ERROR, &err, &len)
			if xyz == 0 && err == 0 {
				return
			}
			if write_result {
				if xyz == 0 {
					wrap_error(err)!
					return
				}
				return
			}
			return err_timed_out
		}
		wrap_error(ecode)!
	}
}

// Select waits for an io operation (specified by parameter `test`) to be available
fn select(handle int, test Select, timeout time.Duration) !bool {
	set := C.fd_set{}

	C.FD_ZERO(&set)
	C.FD_SET(handle, &set)

	seconds := timeout / time.second
	microseconds := time.Duration(timeout - (seconds * time.second)).microseconds()

	mut tt := C.timeval{
		tv_sec:  u64(seconds)
		tv_usec: u64(microseconds)
	}

	mut timeval_timeout := &tt

	// infinite timeout is signaled by passing null as the timeout to
	// select
	if timeout == infinite_timeout {
		timeval_timeout = &C.timeval(unsafe { nil })
	}

	match test {
		.read {
			socket_error(C.select(handle + 1, &set, C.NULL, C.NULL, timeval_timeout))!
		}
		.write {
			socket_error(C.select(handle + 1, C.NULL, &set, C.NULL, timeval_timeout))!
		}
		.except {
			socket_error(C.select(handle + 1, C.NULL, C.NULL, &set, timeval_timeout))!
		}
	}

	return C.FD_ISSET(handle, &set) != 0
}

@[inline]
fn select_deadline(handle int, test Select, deadline time.Time) !bool {
	// if we have a 0 deadline here then the timeout that was passed was infinite...
	infinite := deadline.unix() == 0
	for infinite || time.now() <= deadline {
		timeout := if infinite { infinite_timeout } else { deadline - time.now() }
		ready := select(handle, test, timeout) or {
			if err.code() == C.EINTR {
				// errno is 4, Spurious wakeup from signal, keep waiting
				continue
			}

			// NOT a spurious wakeup
			return err
		}

		return ready
	}

	// Deadline elapsed
	return err_timed_out
}

// wait_for_common wraps the common wait code
fn wait_for_common(handle int, deadline time.Time, timeout time.Duration, test Select) ! {
	// Convert timeouts to deadlines
	real_deadline := if timeout == infinite_timeout {
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
