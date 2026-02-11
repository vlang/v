// BEAM Backend: Use gen_tcp accept loop instead of picoev.
// BEAM: see vbeam_rt for native BEAM server patterns.
//
// On BEAM, event-driven I/O is handled natively by the VM's scheduler:
//   - gen_tcp:listen/2 + gen_tcp:accept/1 for TCP servers
//   - gen_udp:open/1 for UDP servers
//   - ranch/cowboy for production HTTP servers
//
// picoev is a C event loop library (select/kqueue/epoll). These are no-ops on BEAM
// since the BEAM VM provides its own lightweight process scheduling and I/O multiplexing.
// All event loop functions return safe defaults.
module picoev

// max_fds is the maximum number of file descriptors that can be managed.
// BEAM: not applicable since BEAM manages FDs internally. Kept for API compatibility.
pub const max_fds = 1024

// SelectLoop is the BEAM stub for the select event loop backend.
// BEAM: no-op. Use gen_tcp with {active, once} for event-driven I/O.
pub struct SelectLoop {
mut:
	id  int
	now i64
}

// KqueueLoop is the BEAM stub for the kqueue event loop backend (macOS/BSD).
// BEAM: no-op. BEAM VM handles kqueue internally for its schedulers.
@[heap]
pub struct KqueueLoop {
mut:
	id          int
	now         i64
	kq_id       int
	changed_fds int
}

// EpollLoop is the BEAM stub for the epoll event loop backend (Linux).
// BEAM: no-op. BEAM VM handles epoll internally for its schedulers.
@[heap]
pub struct EpollLoop {
mut:
	id      int
	now     i64
	epoll_fd int
}

type LoopType = EpollLoop | KqueueLoop | SelectLoop

// create_select_loop creates a new SelectLoop struct.
// BEAM: returns stub struct. Use gen_tcp for real I/O on BEAM.
pub fn create_select_loop(id int) !&SelectLoop {
	return &SelectLoop{
		id: id
	}
}

// create_kqueue_loop creates a new KqueueLoop struct.
// BEAM: returns stub struct. BEAM VM handles kqueue internally.
pub fn create_kqueue_loop(id int) !&KqueueLoop {
	return &KqueueLoop{
		id: id
	}
}

// create_epoll_loop creates a new EpollLoop struct.
// BEAM: returns stub struct. BEAM VM handles epoll internally.
pub fn create_epoll_loop(id int) !&EpollLoop {
	return &EpollLoop{
		id: id
	}
}

// update_events updates the events associated with a file descriptor.
// BEAM: no-op (stores event flags for API compatibility). Use inet:setopts/2 on BEAM.
@[direct_array_access]
fn (mut pv Picoev) update_events(fd int, events int) int {
	assert fd < max_fds
	pv.file_descriptors[fd].events = u32(events & picoev_readwrite)
	return 0
}

// poll_once performs a single iteration of the event loop.
// BEAM: no-op. On BEAM, use receive loops or gen_server handle_info for event handling.
@[direct_array_access]
fn (mut pv Picoev) poll_once(max_wait_in_sec int) int {
	// BEAM: event handling uses Erlang's receive loop, gen_tcp active mode, or ranch acceptor pools.
	return 0
}

// --- socket_util.c.v stubs ---

// get_time returns the current time in platform-specific units.
// BEAM: returns 0. Use erlang:monotonic_time/0 for real timestamps.
@[inline]
fn get_time() i64 {
	// BEAM: use erlang:monotonic_time() or os:system_time() for real timestamps.
	return i64(0)
}

// accept accepts a new connection on a listening socket.
// BEAM: returns -1. Use gen_tcp:accept/1 on BEAM.
@[inline]
fn accept(fd int) int {
	// BEAM: use gen_tcp:accept(ListenSocket) for accepting connections.
	return -1
}

// close_socket closes a socket file descriptor.
// BEAM: no-op. Use gen_tcp:close/1 on BEAM.
@[inline]
fn close_socket(fd int) {
	// BEAM: use gen_tcp:close(Socket) to close connections.
}

// setup_sock sets up socket options (non-blocking, etc.).
// BEAM: not applicable. BEAM sockets are non-blocking by default via the scheduler.
@[inline]
fn setup_sock(fd int) ! {
	return error('BEAM: socket setup not needed. BEAM VM manages socket options internally.')
}

// req_read reads data from a socket.
// BEAM: returns -1. Use gen_tcp:recv/2,3 or active mode on BEAM.
@[inline]
fn req_read(fd int, buffer &u8, max_len int, offset int) int {
	// BEAM: use gen_tcp:recv(Socket, Length) or {active, once} for data reception.
	return -1
}

// fatal_socket_error checks if a socket error is fatal.
// BEAM: returns true since raw socket operations are not available.
fn fatal_socket_error(fd int) bool {
	// BEAM: raw socket error checking not applicable.
	return true
}

// listen creates a listening tcp socket.
// BEAM: use gen_tcp:listen/2 instead.
fn listen(config Config) !int {
	return error('BEAM: use gen_tcp:listen(Port, Options) for TCP servers.')
}
