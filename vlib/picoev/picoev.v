module picoev

import net
import picohttpparser
import time

// maximum size of the event queue.
pub const max_queue = 4096

// event for incoming data ready to be read on a socket.
pub const picoev_read = 1

// event for socket ready for writing.
pub const picoev_write = 2

// event indicating a timeout has occurred.
pub const picoev_timeout = 4

// flag for adding a file descriptor to the event loop.
pub const picoev_add = 0x40000000

// flag for removing a file descriptor from the event loop.
pub const picoev_del = 0x20000000

// event read/write.
pub const picoev_readwrite = 3

// Target is a data representation of everything that needs to be associated with a single file descriptor (connection).
pub struct Target {
pub mut:
	fd      int // file descriptor
	loop_id int = -1
	events  u32
	cb      fn (int, int, voidptr) = unsafe { nil }
	// used internally by the kqueue implementation
	backend int
}

// Config configures the Picoev instance with server settings and callbacks.
pub struct Config {
pub:
	port         int = 8080
	cb           fn (voidptr, picohttpparser.Request, mut picohttpparser.Response)         = unsafe { nil }
	err_cb       fn (voidptr, picohttpparser.Request, mut picohttpparser.Response, IError) = default_error_callback
	raw_cb       fn (mut Picoev, int, int) = unsafe { nil }
	user_data    voidptr                   = unsafe { nil }
	timeout_secs int                       = 8
	max_headers  int                       = 100
	max_read     int                       = 4096
	max_write    int                       = 8192
	family       net.AddrFamily            = .ip6
	host         string
}

// Core structure for managing the event loop and connections.
// Contains event loop, file descriptor table, timeouts, buffers, and configuration.
@[heap]
pub struct Picoev {
	cb             fn (voidptr, picohttpparser.Request, mut picohttpparser.Response)         = unsafe { nil }
	error_callback fn (voidptr, picohttpparser.Request, mut picohttpparser.Response, IError) = default_error_callback
	raw_callback   fn (mut Picoev, int, int) = unsafe { nil }

	timeout_secs int
	max_headers  int = 100
	max_read     int = 4096
	max_write    int = 8192
mut:
	loop             &LoopType = unsafe { nil }
	file_descriptors [4096]&Target // TODO: use max_fds here, instead of the hardcoded size, when the compiler allows it
	timeouts         map[int]i64
	num_loops        int

	buf &u8 = unsafe { nil }
	idx [max_fds]int
	out &u8 = unsafe { nil }

	date string
pub:
	user_data voidptr = unsafe { nil }
}

// init fills the `file_descriptors` array.
pub fn (mut pv Picoev) init() {
	// assert max_fds > 0
	pv.num_loops = 0
	for i in 0 .. max_fds {
		pv.file_descriptors[i] = &Target{}
	}
}

// add a file descriptor to the event loop.
@[direct_array_access]
pub fn (mut pv Picoev) add(fd int, events int, timeout int, callback voidptr) int {
	if pv == unsafe { nil } || fd < 0 || fd >= max_fds {
		return -1 // Invalid arguments
	}
	mut target := pv.file_descriptors[fd]
	target.fd = fd
	target.cb = callback
	target.loop_id = pv.loop.id
	target.events = 0
	if pv.update_events(fd, events | picoev_add) != 0 {
		if pv.delete(fd) != 0 {
			elog('Error during del')
		}
		return -1
	}
	pv.set_timeout(fd, timeout)
	return 0
}

// remove a file descriptor from the event loop.
@[direct_array_access]
pub fn (mut pv Picoev) delete(fd int) int {
	if fd < 0 || fd >= max_fds {
		return -1 // Invalid fd
	}
	mut target := pv.file_descriptors[fd]
	trace_fd('remove ${fd}')
	if pv.update_events(fd, picoev_del) != 0 {
		elog('Error during update_events. event: `picoev.picoev_del`')
		return -1
	}
	pv.set_timeout(fd, 0)
	target.loop_id = -1
	target.fd = 0
	target.cb = unsafe { nil } // Clear callback to prevent accidental invocations
	return 0
}

fn (mut pv Picoev) loop_once(max_wait_in_sec int) int {
	pv.loop.now = get_time()
	if pv.poll_once(max_wait_in_sec) != 0 {
		elog('Error during poll_once')
		return -1
	}
	if max_wait_in_sec == 0 {
		// If no waiting, skip timeout handling for potential performance optimization
		return 0
	}
	// Update loop start time again if waiting occurred
	pv.loop.now = get_time()
	pv.handle_timeout()
	return 0
}

// set_timeout sets the timeout in seconds for a file descriptor. If a timeout occurs
// the file descriptors target callback is called with a timeout event.
@[direct_array_access; inline]
fn (mut pv Picoev) set_timeout(fd int, secs int) {
	assert fd < max_fds
	if secs == 0 {
		pv.timeouts.delete(fd)
	} else {
		pv.timeouts[fd] = pv.loop.now + secs
	}
}

// handle_timeout loops over all file descriptors and removes them from the loop
// if they are timed out. Also the file descriptors target callback is called with a
// timeout event.
@[direct_array_access; inline]
fn (mut pv Picoev) handle_timeout() {
	mut to_remove := []int{}
	for fd, timeout in pv.timeouts {
		if timeout <= pv.loop.now {
			to_remove << fd
		}
	}
	for fd in to_remove {
		target := pv.file_descriptors[fd]
		assert target.loop_id == pv.loop.id
		pv.timeouts.delete(fd)
		unsafe { target.cb(fd, picoev_timeout, &pv) }
	}
}

// accept_callback accepts a new connection from `listen_fd` and adds it to the event loop.
fn accept_callback(listen_fd int, events int, cb_arg voidptr) {
	mut pv := unsafe { &Picoev(cb_arg) }
	accepted_fd := accept(listen_fd)
	if accepted_fd == -1 {
		if fatal_socket_error(accepted_fd) == false {
			return
		}
		elog('Error during accept')
		return
	}
	if accepted_fd >= max_fds {
		// should never happen
		elog('Error during accept, accepted_fd >= max_fd')
		close_socket(accepted_fd)
		return
	}
	trace_fd('accept ${accepted_fd}')
	setup_sock(accepted_fd) or {
		elog('setup_sock failed, fd: ${accepted_fd}, listen_fd: ${listen_fd}, err: ${err.code()}')
		pv.error_callback(pv.user_data, picohttpparser.Request{}, mut &picohttpparser.Response{},
			err)
		close_socket(accepted_fd) // Close fd on failure
		return
	}
	pv.add(accepted_fd, picoev_read, pv.timeout_secs, raw_callback)
}

// close_conn closes the socket `fd` and removes it from the loop.
@[inline]
pub fn (mut pv Picoev) close_conn(fd int) {
	if pv.delete(fd) != 0 {
		elog('Error during del')
	}
	close_socket(fd)
}

// raw_callback handles raw events (read, write, timeout) for a file descriptor.
@[direct_array_access]
fn raw_callback(fd int, events int, context voidptr) {
	mut pv := unsafe { &Picoev(context) }
	defer {
		pv.idx[fd] = 0
	}
	if events & picoev_timeout != 0 {
		trace_fd('timeout ${fd}')
		if !isnil(pv.raw_callback) {
			pv.raw_callback(mut pv, fd, events)
			return
		}
		pv.close_conn(fd)
		return
	} else if events & picoev_read != 0 {
		pv.set_timeout(fd, pv.timeout_secs)
		if !isnil(pv.raw_callback) {
			pv.raw_callback(mut pv, fd, events)
			return
		}
		mut request_buffer := pv.buf
		unsafe {
			request_buffer += fd * pv.max_read // pointer magic
		}
		mut req := picohttpparser.Request{}
		// Response init
		mut response_buffer := pv.out
		unsafe {
			response_buffer += fd * pv.max_write // pointer magic
		}
		mut res := picohttpparser.Response{
			fd:        fd
			buf_start: response_buffer
			buf:       response_buffer
			date:      pv.date.str
		}
		for {
			// Request parsing loop
			r := req_read(fd, request_buffer, pv.max_read, pv.idx[fd]) // Get data from socket
			if r == 0 {
				// connection closed by peer
				pv.close_conn(fd)
				return
			} else if r == -1 {
				if fatal_socket_error(fd) == false {
					return
				}
				elog('Error during req_read')
				// fatal error
				pv.close_conn(fd)
				return
			}
			pv.idx[fd] += r
			mut s := unsafe { tos(request_buffer, pv.idx[fd]) }
			pret := req.parse_request(s) or {
				// Parse error
				pv.error_callback(pv.user_data, req, mut &res, err)
				return
			}
			if pret > 0 { // Success
				break
			}
			assert pret == -2
			// request is incomplete, continue the loop
			if pv.idx[fd] == sizeof(request_buffer) {
				pv.error_callback(pv.user_data, req, mut &res, error('RequestIsTooLongError'))
				return
			}
		}
		// Callback (should call .end() itself)
		pv.cb(pv.user_data, req, mut &res)
	} else if events & picoev_write != 0 {
		pv.set_timeout(fd, pv.timeout_secs)
		if !isnil(pv.raw_callback) {
			pv.raw_callback(mut pv, fd, events)
			return
		}
	}
}

fn default_error_callback(data voidptr, req picohttpparser.Request, mut res picohttpparser.Response, error IError) {
	elog('picoev: ${error}')
	res.end()
}

// new creates a `Picoev` struct and initializes the main loop.
pub fn new(config Config) !&Picoev {
	listening_socket_fd := listen(config) or {
		elog('Error during listen: ${err}')
		return err
	}
	mut pv := &Picoev{
		num_loops:      1
		cb:             config.cb
		error_callback: config.err_cb
		raw_callback:   config.raw_cb
		user_data:      config.user_data
		timeout_secs:   config.timeout_secs
		max_headers:    config.max_headers
		max_read:       config.max_read
		max_write:      config.max_write
	}
	if isnil(pv.raw_callback) {
		pv.buf = unsafe { malloc_noscan(max_fds * config.max_read + 1) }
		pv.out = unsafe { malloc_noscan(max_fds * config.max_write + 1) }
	}
	// epoll on linux
	// kqueue on macos and bsd
	// select on windows and others
	$if linux || termux {
		pv.loop = create_epoll_loop(0) or { panic(err) }
	} $else $if freebsd || macos || openbsd {
		pv.loop = create_kqueue_loop(0) or { panic(err) }
	} $else {
		pv.loop = create_select_loop(0) or { panic(err) }
	}
	if pv.loop == unsafe { nil } {
		elog('Failed to create loop')
		close_socket(listening_socket_fd)
		return unsafe { nil }
	}
	pv.init()
	pv.add(listening_socket_fd, picoev_read, 0, accept_callback)
	return pv
}

// serve starts the event loop for accepting new connections.
// See also picoev.new().
pub fn (mut pv Picoev) serve() {
	spawn update_date_string(mut pv)
	for {
		pv.loop_once(1)
	}
}

// update_date updates the date field of the Picoev instance every second for HTTP headers.
fn update_date_string(mut pv Picoev) {
	for {
		// get GMT (UTC) time for the HTTP Date header
		gmt := time.utc()
		pv.date = gmt.http_header_string()
		time.sleep(time.second)
	}
}
