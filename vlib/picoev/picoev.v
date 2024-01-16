module picoev

import net
import picohttpparser
import time

pub const max_fds = 1024

pub const max_queue = 4096

// events
pub const picoev_read = 1

pub const picoev_write = 2

pub const picoev_timeout = 4

pub const picoev_add = 0x40000000

pub const picoev_del = 0x20000000

pub const picoev_readwrite = 3

// Target is a data representation of everything that needs to be associated with a single
// file descriptor (connection)
pub struct Target {
pub mut:
	fd      int
	loop_id int = -1
	events  u32
	cb      fn (int, int, voidptr) = unsafe { nil }
	// used internally by the kqueue implementation
	backend int
}

pub struct Config {
pub:
	port         int = 8080
	cb           fn (voidptr, picohttpparser.Request, mut picohttpparser.Response) = unsafe { nil }
	err_cb       fn (voidptr, picohttpparser.Request, mut picohttpparser.Response, IError) = default_err_cb
	raw_cb       fn (mut Picoev, int, int) = unsafe { nil }
	user_data    voidptr        = unsafe { nil }
	timeout_secs int            = 8
	max_headers  int            = 100
	max_read     int            = 4096
	max_write    int            = 8192
	family       net.AddrFamily = .ip
	host         string = 'localhost'
}

@[heap]
pub struct Picoev {
	cb     fn (voidptr, picohttpparser.Request, mut picohttpparser.Response) = unsafe { nil }
	err_cb fn (voidptr, picohttpparser.Request, mut picohttpparser.Response, IError) = default_err_cb
	raw_cb fn (mut Picoev, int, int) = unsafe { nil }

	timeout_secs int
	max_headers  int = 100
	max_read     int = 4096
	max_write    int = 8192
mut:
	loop             &LoopType = unsafe { nil }
	file_descriptors [max_fds]&Target
	timeouts         map[int]i64
	num_loops        int

	buf &u8 = unsafe { nil }
	idx [1024]int
	out &u8 = unsafe { nil }

	date string
pub:
	user_data voidptr = unsafe { nil }
}

// init fills the `file_descriptors` array
pub fn (mut pv Picoev) init() {
	assert picoev.max_fds > 0

	pv.num_loops = 0

	for i in 0 .. picoev.max_fds {
		pv.file_descriptors[i] = &Target{}
	}
}

// add adds a file descriptor to the loop
@[direct_array_access]
pub fn (mut pv Picoev) add(fd int, events int, timeout int, cb voidptr) int {
	if pv == unsafe { nil } || fd < 0 || fd >= picoev.max_fds {
		return -1 // Invalid arguments
	}

	mut target := pv.file_descriptors[fd]
	target.fd = fd
	target.cb = cb
	target.loop_id = pv.loop.id
	target.events = 0

	if pv.update_events(fd, events | picoev.picoev_add) != 0 {
		if pv.del(fd) != 0 {
			eprintln('Error during del')
		}
		close_socket(fd) // Close fd on failure
		return -1
	}

	pv.set_timeout(fd, timeout)
	return 0
}

// del removes a file descriptor from the loop
@[direct_array_access]
pub fn (mut pv Picoev) del(fd int) int {
	if fd < 0 || fd >= picoev.max_fds {
		return -1 // Invalid fd
	}

	mut target := pv.file_descriptors[fd]

	$if trace_fd ? {
		eprintln('delete ${fd}')
	}

	if pv.update_events(fd, picoev.picoev_del) != 0 {
		eprintln('Error during update_events')
		return -1
	}

	pv.set_timeout(fd, 0)
	target.loop_id = -1
	target.fd = 0
	target.cb = unsafe { nil } // Clear callback to prevent accidental invocations
	return 0
}

fn (mut pv Picoev) loop_once(max_wait_in_sec int) int {
	pv.loop.now = get_time() // Update loop start time

	if pv.poll_once(max_wait_in_sec) != 0 {
		eprintln('Error during poll_once')
		return -1
	}

	if max_wait_in_sec != 0 {
		pv.loop.now = get_time() // Update loop start time again if waiting occurred
	} else {
		// If no waiting, skip timeout handling for potential performance optimization
		return 0
	}

	pv.handle_timeout()
	return 0
}

// set_timeout sets the timeout in seconds for a file descriptor. If a timeout occurs
// the file descriptors target callback is called with a timeout event
@[direct_array_access; inline]
fn (mut pv Picoev) set_timeout(fd int, secs int) {
	assert fd < picoev.max_fds
	if secs != 0 {
		pv.timeouts[fd] = pv.loop.now + secs
	} else {
		pv.timeouts.delete(fd)
	}
}

// handle_timeout loops over all file descriptors and removes them from the loop
// if they are timed out. Also the file descriptors target callback is called with a
// timeout event
@[direct_array_access; inline]
fn (mut pv Picoev) handle_timeout() {
	for fd, timeout in pv.timeouts {
		if timeout <= pv.loop.now {
			target := pv.file_descriptors[fd]
			assert target.loop_id == pv.loop.id
			if target.cb != unsafe { nil } { // Check for valid callback
				unsafe { target.cb(fd, picoev.picoev_timeout, &pv) }
			}
			pv.timeouts.delete(fd)
		}
	}
}

// accept_callback accepts a new connection from `listen_fd` and adds it to the loop
fn accept_callback(listen_fd int, events int, cb_arg voidptr) {
	mut pv := unsafe { &Picoev(cb_arg) }
	accepted_fd := accept(listen_fd)
	if accepted_fd == -1 {
		eprintln('Error during accept')
		return
	}

	if accepted_fd >= picoev.max_fds {
		// should never happen
		close_socket(accepted_fd)
		return
	}

	$if trace_fd ? {
		eprintln('accept ${accepted_fd}')
	}

	setup_sock(accepted_fd) or {
		eprintln('setup_sock failed, fd: ${accepted_fd}, listen_fd: ${listen_fd}, err: ${err.code()}')
		pv.err_cb(pv.user_data, picohttpparser.Request{}, mut &picohttpparser.Response{},
			err)
		close_socket(accepted_fd) // Close fd on failure
		return
	}
	pv.add(accepted_fd, picoev.picoev_read, pv.timeout_secs, raw_callback)
}

// close_conn closes the socket `fd` and removes it from the loop
@[inline]
pub fn (mut pv Picoev) close_conn(fd int) {
	if pv.del(fd) != 0 {
		eprintln('Error during del')
	}
	close_socket(fd) // Close fd regardless of del outcome
}

@[direct_array_access]
fn raw_callback(fd int, events int, context voidptr) {
	mut pv := unsafe { &Picoev(context) }
	defer {
		pv.idx[fd] = 0
	}

	if events & picoev.picoev_timeout != 0 {
		$if trace_fd ? {
			eprintln('timeout ${fd}')
		}

		if !isnil(pv.raw_cb) {
			pv.raw_cb(mut pv, fd, events)
			return
		}

		pv.close_conn(fd)
		return
	} else if events & picoev.picoev_read != 0 {
		pv.set_timeout(fd, pv.timeout_secs)
		if !isnil(pv.raw_cb) {
			pv.raw_cb(mut pv, fd, events)
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
			fd: fd
			buf_start: response_buffer
			buf: response_buffer
			date: pv.date.str
		}

		for {
			// Request parsing loop
			r := req_read(fd, request_buffer, pv.max_read, pv.idx[fd]) // Get data from socket
			if r == 0 {
				// connection closed by peer
				pv.close_conn(fd)
				return
			} else if r == -1 {
				eprintln('Error during req_read')
				close_socket(fd) // Close fd on failure
				return
			}
			pv.idx[fd] += r

			mut s := unsafe { tos(request_buffer, pv.idx[fd]) }
			pret := req.parse_request(s) or {
				// Parse error
				pv.err_cb(pv.user_data, req, mut &res, err)
				return
			}
			if pret > 0 { // Success
				break
			}

			assert pret == -2
			// request is incomplete, continue the loop
			if pv.idx[fd] == sizeof(request_buffer) {
				pv.err_cb(pv.user_data, req, mut &res, error('RequestIsTooLongError'))
				return
			}
		}

		// Callback (should call .end() itself)
		pv.cb(pv.user_data, req, mut &res)
	} else if events & picoev.picoev_write != 0 {
		pv.set_timeout(fd, pv.timeout_secs)
		if !isnil(pv.raw_cb) {
			pv.raw_cb(mut pv, fd, events)
			return
		}
	}
}

fn default_err_cb(data voidptr, req picohttpparser.Request, mut res picohttpparser.Response, error IError) {
	eprintln('picoev: ${error}')
	res.end()
}

// new creates a `Picoev` struct and initializes the main loop
fn new(config Config) &Picoev {
	listening_socket_fd := listen(config) or {
		eprintln('Error during listen: ${err}')
		return unsafe { nil }
	}

	mut pv := &Picoev{
		num_loops: 1
		cb: config.cb
		err_cb: config.err_cb
		raw_cb: config.raw_cb
		user_data: config.user_data
		timeout_secs: config.timeout_secs
		max_headers: config.max_headers
		max_read: config.max_read
		max_write: config.max_write
	}

	if isnil(pv.raw_cb) {
		pv.buf = unsafe { malloc_noscan(picoev.max_fds * config.max_read + 1) }
		pv.out = unsafe { malloc_noscan(picoev.max_fds * config.max_write + 1) }
	}

	// epoll for linux
	// kqueue for macos and bsd
	// select for windows and others
	$if linux {
		pv.loop = create_epoll_loop(0) or { panic(err) }
	} $else $if freebsd || macos {
		pv.loop = create_kqueue_loop(0) or { panic(err) }
	} $else {
		pv.loop = create_select_loop(0) or { panic(err) }
	}

	if pv.loop == unsafe { nil } {
		eprintln('Failed to create loop')
		close_socket(listening_socket_fd) // Close socket on failure
		return unsafe { nil }
	}

	pv.init()

	pv.add(listening_socket_fd, picoev.picoev_read, 0, accept_callback)
	return pv
}

// serve starts the Picoev server
pub fn (mut pv Picoev) serve() {
	for {
		pv.loop_once(1)

		// Update date within the main loop
		pv.date = update_date_string()
	}
}

// update_date updates `date` on `pv` every second.
fn update_date_string() string {
	gmt := time.utc() // slow
	mut date_string := gmt.strftime('---, %d --- %Y %H:%M:%S GMT')
	date_string = date_string.replace_once('---', gmt.weekday_str())
	date_string = date_string.replace_once('---', gmt.smonth())
	return date_string
}
