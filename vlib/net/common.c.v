module net

import time

// no_deadline disables the absolute deadline; the configured socket timeout still applies.
const no_deadline = time.unix(0)

// no_timeout disables the relative socket timeout. An explicit deadline still applies;
// with no deadline, socket waits block until ready or an error occurs.
pub const no_timeout = time.Duration(0)

// infinite_timeout disables the relative socket timeout, like no_timeout.
// An explicit deadline still applies.
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
	mut res := 0
	$if vlibc ? {
		res = vlibc_shutdown_result(handle, int(config.how))
	}
	$if !vlibc ? {
		res = C.shutdown(handle, int(config.how))
	}
	$if !net_nonblocking_sockets ? {
		return res
	} $else {
		if res == 0 {
			return 0
		}
		// With -d vlibc, res already encodes -errno (C.errno is untouched
		// by raw syscalls), so recover the code from res instead.
		ecode := $if vlibc ? {
			-res
		} $else {
			error_code()
		}
		if (is_windows && ecode == int(error_ewouldblock)) || (!is_windows && res == -1
			&& ecode in [int(error_einprogress), int(error_eagain), C.EINTR]) {
			write_result := select_deadline(handle, .write, time.now().add(connect_timeout)) or {
				false
			}
			mut err := i32(0) // SO_ERROR is a C `int`; use i32 storage so &err is int* and sizeof is 4
			mut xyz := 0
			$if vlibc ? {
				// SO_ERROR goes through the vlibc syscall on Linux; other
				// vlibc targets keep the libc call below.
				$if linux {
					xyz, err = vlibc_get_so_error(handle)
				} $else {
					len := sizeof(err)
					xyz = C.getsockopt(handle, C.SOL_SOCKET, C.SO_ERROR, &err, &len)
				}
			} $else {
				len := sizeof(err)
				xyz = C.getsockopt(handle, C.SOL_SOCKET, C.SO_ERROR, &err, &len)
			}
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
			mut err := i32(0) // SO_ERROR is a C `int`; use i32 storage so &err is int* and sizeof is 4
			mut xyz := 0
			$if vlibc ? {
				// SO_ERROR goes through the vlibc syscall on Linux; other
				// vlibc targets keep the libc call below.
				$if linux {
					xyz, err = vlibc_get_so_error(handle)
				} $else {
					len := sizeof(err)
					xyz = C.getsockopt(handle, C.SOL_SOCKET, C.SO_ERROR, &err, &len)
				}
			} $else {
				len := sizeof(err)
				xyz = C.getsockopt(handle, C.SOL_SOCKET, C.SO_ERROR, &err, &len)
			}
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
			return error_with_code(err_timed_out.msg(), err_timed_out.code())
		}
		wrap_error(ecode)!
	}
}

// select_one issues a single select(2) wait and returns the ready count.
// Under `-d vlibc` on macOS it goes through the raw vlibc select syscall
// (macOS is the only vlibc target with an in-tree select number); every
// other target keeps the libc call below, which also serves vlibc-created
// fds since they live in the same process table. Raw-syscall errors keep
// the socket_error message/code shape, so the EINTR-restart check in
// select_deadline_chunked keeps working.
fn select_one(handle int, r &C.fd_set, w &C.fd_set, x &C.fd_set, tv &C.timeval, sec i64, usec i64, has_timeout bool) !int {
	$if vlibc ? {
		$if macos {
			n, e := vlibc_select_wait(handle, voidptr(r), voidptr(w), voidptr(x), sec, usec,
				has_timeout)
			if e != .enoerror {
				return error_with_code('net: socket error: ${int(e)}', int(e))
			}
			return int(n)
		} $else {
			// No select/pselect6 number in-tree for this vlibc target.
			return socket_error(C.select(handle, r, w, x, tv))
		}
	} $else {
		return socket_error(C.select(handle, r, w, x, tv))
	}
}

// Select waits for an io operation (specified by parameter `test`) to be available
fn select(handle int, test Select, timeout time.Duration) !bool {
	set := C.fd_set{}

	C.FD_ZERO(&set)
	C.FD_SET(handle, &set)

	// infinite timeout is signaled by passing null as the timeout to select.
	if timeout == infinite_timeout {
		match test {
			.read {
				select_one(handle + 1, &set, unsafe { nil }, unsafe { nil }, unsafe { nil }, 0, 0, false)!
			}
			.write {
				select_one(handle + 1, unsafe { nil }, &set, unsafe { nil }, unsafe { nil }, 0, 0, false)!
			}
			.except {
				select_one(handle + 1, unsafe { nil }, unsafe { nil }, &set, unsafe { nil }, 0, 0, false)!
			}
		}
	} else {
		seconds := timeout / time.second
		microseconds := time.Duration(timeout - (seconds * time.second)).microseconds()
		tt := C.timeval{
			tv_sec:  u64(seconds)
			tv_usec: u64(microseconds)
		}
		match test {
			.read {
				select_one(handle + 1, &set, unsafe { nil }, unsafe { nil }, &tt, seconds, microseconds, true)!
			}
			.write {
				select_one(handle + 1, unsafe { nil }, &set, unsafe { nil }, &tt, seconds, microseconds, true)!
			}
			.except {
				select_one(handle + 1, unsafe { nil }, unsafe { nil }, &set, &tt, seconds, microseconds, true)!
			}
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
			infinite_timeout
		} else if chunked {
			max_wait
		} else {
			deadline - now
		}
		ready := select(handle, test, timeout) or {
			if err.code() == C.EINTR {
				// errno is 4, Spurious wakeup from signal, keep waiting
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
	return error_with_code(err_timed_out.msg(), err_timed_out.code())
}

// deadline_set is whether a deadline was set: anything but the zero `time.Time{}` a connection
// starts with and the `time.unix(0)` that select_deadline reads as infinite. A deadline already in
// the past, even before 1970, is set: it is due.
fn deadline_set(deadline time.Time) bool {
	return !deadline.is_zero() && deadline != time.unix(0)
}

// wait_for_common wraps the common wait code
fn wait_for_common(handle int, deadline time.Time, timeout time.Duration, test Select) ! {
	// Convert timeouts to deadlines. A deadline the caller set is the request for this wait,
	// so it wins over the connection's timeout (a UdpConn's is 100 ms by default).
	real_deadline := if deadline_set(deadline) {
		deadline
	} else if timeout == infinite_timeout {
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

	return error_with_code(err_timed_out.msg(), err_timed_out.code())
}

// wait_for_write waits for a write io operation to be available
fn wait_for_write(handle int, deadline time.Time, timeout time.Duration) ! {
	return wait_for_common(handle, deadline, timeout, .write)
}

// wait_for_read waits for a read io operation to be available
fn wait_for_read(handle int, deadline time.Time, timeout time.Duration) ! {
	return wait_for_common(handle, deadline, timeout, .read)
}
