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
fn shutdown(handle int) ? {
	$if windows {
		C.shutdown(handle, C.SD_BOTH)
		socket_error(C.closesocket(handle)) ?
	} $else {
		C.shutdown(handle, C.SHUT_RDWR)
		socket_error(C.close(handle)) ?
	}
}

// Select waits for an io operation (specified by parameter `test`) to be available
fn @select(handle int, test Select, timeout time.Duration) ?bool {
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
			socket_error(C.@select(handle + 1, &set, C.NULL, C.NULL, timeval_timeout)) ?
		}
		.write {
			socket_error(C.@select(handle + 1, C.NULL, &set, C.NULL, timeval_timeout)) ?
		}
		.except {
			socket_error(C.@select(handle + 1, C.NULL, C.NULL, &set, timeval_timeout)) ?
		}
	}

	return C.FD_ISSET(handle, &set)
}

// select_with_retry will retry the select if select is failing
// due to interrupted system call. This can happen on signals
// for example the GC Boehm uses signals internally on garbage
// collection
[inline]
fn select_with_retry(handle int, test Select, timeout time.Duration) ?bool {
	mut retries := 10
	for retries > 0 {
		ready := @select(handle, test, timeout) or {
			if err.code() == 4 {
				// signal! lets retry max 10 times
				// suspend thread with sleep to let the gc get
				// cycles in the case the Bohem gc is interupting
				time.sleep(1 * time.millisecond)
				retries -= 1
				continue
			}
			// we got other error
			return err
		}
		return ready
	}
	return error('failed to @select more that three times due to interrupted system call')
}

// wait_for_common wraps the common wait code
fn wait_for_common(handle int, deadline time.Time, timeout time.Duration, test Select) ? {
	if deadline.unix == 0 {
		// do not accept negative timeout
		if timeout < 0 {
			return err_timed_out
		}
		ready := select_with_retry(handle, test, timeout) ?
		if ready {
			return
		}
		return err_timed_out
	}
	// Convert the deadline into a timeout
	// and use that
	d_timeout := deadline.unix - time.now().unix
	if d_timeout < 0 {
		// deadline is in the past so this has already
		// timed out
		return err_timed_out
	}
	ready := select_with_retry(handle, test, timeout) ?
	if ready {
		return
	}
	return err_timed_out
}

// wait_for_write waits for a write io operation to be available
fn wait_for_write(handle int, deadline time.Time, timeout time.Duration) ? {
	return wait_for_common(handle, deadline, timeout, .write)
}

// wait_for_read waits for a read io operation to be available
fn wait_for_read(handle int, deadline time.Time, timeout time.Duration) ? {
	return wait_for_common(handle, deadline, timeout, .read)
}
