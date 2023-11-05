module picoev

import picohttpparser
import time

pub const (
	max_fds          = 1024
	max_queue        = 4096

	// events
	picoev_read      = 1
	picoev_write     = 2
	picoev_timeout   = 4
	picoev_add       = 0x40000000
	picoev_del       = 0x20000000
	picoev_readwrite = 3 // 1 xor 2
)

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
	raw_cb       fn (voidptr, int) = unsafe { nil }
	user_data    voidptr = unsafe { nil }
	timeout_secs int     = 8
	max_headers  int     = 100
	max_read     int     = 4096
	max_write    int     = 8192
}

[heap]
pub struct Picoev {
	cb        fn (voidptr, picohttpparser.Request, mut picohttpparser.Response) = unsafe { nil }
	err_cb    fn (voidptr, picohttpparser.Request, mut picohttpparser.Response, IError) = default_err_cb
	raw_cb    fn (voidptr, int) = unsafe { nil }
	user_data voidptr = unsafe { nil }

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
[direct_array_access]
pub fn (mut pv Picoev) add(fd int, events int, timeout int, cb voidptr) int {
	assert fd < picoev.max_fds

	mut target := pv.file_descriptors[fd]
	target.fd = fd
	target.cb = cb
	target.loop_id = pv.loop.id
	target.events = 0

	if pv.update_events(fd, events | picoev.picoev_add) != 0 {
		pv.del(fd)
		return -1
	}

	// update timeout
	pv.set_timeout(fd, timeout)
	return 0
}

// del removes a file descriptor from the loop
[direct_array_access]
fn (mut pv Picoev) del(fd int) int {
	assert fd < picoev.max_fds
	mut target := pv.file_descriptors[fd]

	$if trace_fd ? {
		eprintln('delete ${fd}')
	}

	if pv.update_events(fd, picoev.picoev_del) != 0 {
		return -1
	}

	pv.set_timeout(fd, 0)
	target.loop_id = -1
	target.fd = 0
	return 0
}

fn (mut pv Picoev) loop_once(max_wait int) int {
	pv.loop.now = get_time()

	if pv.poll_once(max_wait) != 0 {
		return -1
	}

	if max_wait != 0 {
		pv.loop.now = get_time()
	}

	pv.handle_timeout()
	return 0
}

// set_timeout sets the timeout in seconds for a file descriptor. If a timeout occurs
// the file descriptors target callback is called with a timeout event
[direct_array_access; inline]
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
[direct_array_access; inline]
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
		unsafe { target.cb(fd, picoev.picoev_timeout, &pv) }
	}
}

// accept_callback accepts a new connection from `listen_fd` and adds it to the loop
fn accept_callback(listen_fd int, events int, cb_arg voidptr) {
	mut pv := unsafe { &Picoev(cb_arg) }
	newfd := accept(listen_fd)
	if newfd >= picoev.max_fds {
		// should never happen
		close_socket(newfd)
		return
	}

	$if trace_fd ? {
		eprintln('accept ${newfd}')
	}

	if newfd != -1 {
		setup_sock(newfd) or {
			eprintln('setup_sock failed, fd: ${newfd}, listen_fd: ${listen_fd}, err: ${err.code()}')
			pv.err_cb(pv.user_data, picohttpparser.Request{}, mut &picohttpparser.Response{},
				err)
			return
		}
		pv.add(newfd, picoev.picoev_read, pv.timeout_secs, raw_callback)
	}
}

// close_conn closes the socket `fd` and removes it from the loop
[inline]
pub fn (mut pv Picoev) close_conn(fd int) {
	pv.del(fd)
	close_socket(fd)
}

[direct_array_access]
fn raw_callback(fd int, events int, context voidptr) {
	mut pv := unsafe { &Picoev(context) }
	defer {
		pv.idx[fd] = 0
	}

	if events & picoev.picoev_timeout != 0 {
		$if trace_fd ? {
			eprintln('timeout ${fd}')
		}
		pv.close_conn(fd)
		return
	} else if events & picoev.picoev_read != 0 {
		pv.set_timeout(fd, pv.timeout_secs)
		if !isnil(pv.raw_cb) {
			pv.raw_cb(pv.user_data, fd)
			return
		}

		mut buf := pv.buf
		unsafe {
			buf += fd * pv.max_read // pointer magic
		}
		mut req := picohttpparser.Request{}

		// Response init
		mut out := pv.out
		unsafe {
			out += fd * pv.max_write // pointer magic
		}
		mut res := picohttpparser.Response{
			fd: fd
			buf_start: out
			buf: out
			date: pv.date.str
		}

		for {
			// Request parsing loop
			r := req_read(fd, buf, pv.max_read, pv.idx[fd]) // Get data from socket
			if r == 0 {
				// connection closed by peer
				pv.close_conn(fd)
				return
			} else if r == -1 {
				if fatal_socket_error(fd) == false {
					return
				}

				// fatal error
				pv.close_conn(fd)
				return
			}
			pv.idx[fd] += r

			mut s := unsafe { tos(buf, pv.idx[fd]) }
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
			if pv.idx[fd] == sizeof(buf) {
				pv.err_cb(pv.user_data, req, mut &res, error('RequestIsTooLongError'))
				return
			}
		}

		// Callback (should call .end() itself)
		pv.cb(pv.user_data, req, mut &res)
	}
}

fn default_err_cb(data voidptr, req picohttpparser.Request, mut res picohttpparser.Response, error IError) {
	eprintln('picoev: ${error}')
	res.end()
}

// new creates a `Picoev` struct and initializes the main loop
pub fn new(config Config) &Picoev {
	listen_fd := listen(config)

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

	pv.init()

	pv.add(listen_fd, picoev.picoev_read, 0, accept_callback)
	return pv
}

// serve starts the Picoev server
pub fn (mut pv Picoev) serve() {
	spawn update_date(mut pv)

	for {
		pv.loop_once(1)
	}
}

// update_date updates `date` on `pv` every second.
fn update_date(mut pv Picoev) {
	for {
		// get GMT (UTC) time for the HTTP Date header
		gmt := time.utc()
		mut date := gmt.strftime('---, %d --- %Y %H:%M:%S GMT')
		date = date.replace_once('---', gmt.weekday_str())
		date = date.replace_once('---', gmt.smonth())
		pv.date = date
		time.sleep(time.second)
	}
}
