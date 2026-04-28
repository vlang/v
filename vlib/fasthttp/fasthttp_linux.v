module fasthttp

import net
import sync.stdatomic

#include <sys/epoll.h>
#include <sys/sendfile.h>
#include <sys/stat.h>
#include <netinet/tcp.h>

const epoll_wait_timeout_ms = 100

fn C.accept4(sockfd i32, addr &net.Addr, addrlen &u32, flags i32) i32

fn C.epoll_create1(__flags i32) i32

fn C.epoll_ctl(__epfd i32, __op i32, __fd i32, __event &C.epoll_event) i32

fn C.epoll_wait(__epfd i32, __events &C.epoll_event, __maxevents i32, __timeout i32) i32

fn C.sendfile(out_fd i32, in_fd i32, offset &i64, count usize) i32

fn C.fstat(fd i32, buf &C.stat) i32

@[typedef]
union C.epoll_data_t {
mut:
	ptr voidptr
	fd  int
	u32 u32
	u64 u64
}

struct C.epoll_event {
mut:
	events u32
	data   C.epoll_data_t
}

pub struct Server {
pub:
	family                  net.AddrFamily = .ip6
	port                    int            = 3000
	max_request_buffer_size int            = 8192
	user_data               voidptr
mut:
	listen_fds      []int    = []int{len: max_thread_pool_size, cap: max_thread_pool_size, init: -1}
	epoll_fds       []int    = []int{len: max_thread_pool_size, cap: max_thread_pool_size, init: -1}
	threads         []thread = []thread{len: max_thread_pool_size, cap: max_thread_pool_size}
	request_handler fn (HttpRequest) !HttpResponse @[required]
	running         &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)
	shutting_down   &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)
	stopped         &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(true)
	active_requests &stdatomic.AtomicVal[int]  = stdatomic.new_atomic(0)
}

// new_server creates and initializes a new Server instance.
pub fn new_server(config ServerConfig) !&Server {
	if config.max_request_buffer_size <= 0 {
		return error('max_request_buffer_size must be greater than 0')
	}
	mut server := &Server{
		family:                  config.family
		port:                    config.port
		max_request_buffer_size: config.max_request_buffer_size
		user_data:               config.user_data
		request_handler:         config.handler
		running:                 stdatomic.new_atomic(false)
		shutting_down:           stdatomic.new_atomic(false)
		stopped:                 stdatomic.new_atomic(true)
		active_requests:         stdatomic.new_atomic(0)
	}
	unsafe {
		server.listen_fds.flags.set(.noslices | .noshrink | .nogrow)
		server.epoll_fds.flags.set(.noslices | .noshrink | .nogrow)
		server.threads.flags.set(.noslices | .noshrink | .nogrow)
	}
	return server
}

fn set_blocking(fd int, blocking bool) {
	flags := C.fcntl(fd, C.F_GETFL, 0)
	if flags == -1 {
		// TODO: better error handling
		eprintln(@LOCATION)
		return
	}
	if blocking {
		// This removes the O_NONBLOCK flag from flags and set it.
		C.fcntl(fd, C.F_SETFL, flags & ~C.O_NONBLOCK)
	} else {
		// This adds the O_NONBLOCK flag from flags and set it.
		C.fcntl(fd, C.F_SETFL, flags | C.O_NONBLOCK)
	}
}

fn close_socket(fd int) bool {
	ret := C.close(fd)
	if ret == -1 {
		if C.errno == C.EINTR {
			// Interrupted by signal, retry is safe
			return close_socket(fd)
		}
		eprintln('ERROR: close(fd=${fd}) failed with errno=${C.errno}')
		return false
	}
	return true
}

fn create_server_socket(server Server) int {
	// Create a socket with non-blocking mode
	server_fd := C.socket(server.family, net.SocketType.tcp, 0)
	if server_fd < 0 {
		eprintln(@LOCATION)
		C.perror(c'Socket creation failed')
		return -1
	}

	set_blocking(server_fd, false)

	// Enable SO_REUSEADDR and SO_REUSEPORT
	opt := 1
	if C.setsockopt(server_fd, C.SOL_SOCKET, C.SO_REUSEADDR, &opt, sizeof(opt)) < 0 {
		eprintln(@LOCATION)
		C.perror(c'setsockopt SO_REUSEADDR failed')
		close_socket(server_fd)
		return -1
	}
	if C.setsockopt(server_fd, C.SOL_SOCKET, C.SO_REUSEPORT, &opt, sizeof(opt)) < 0 {
		eprintln(@LOCATION)
		C.perror(c'setsockopt SO_REUSEPORT failed')
		close_socket(server_fd)
		return -1
	}

	addr := if server.family == .ip6 {
		net.new_ip6(u16(server.port), [u8(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]!)
	} else {
		net.new_ip(u16(server.port), [u8(0), 0, 0, 0]!)
	}
	alen := addr.len()
	if C.bind(server_fd, voidptr(&addr), alen) < 0 {
		eprintln(@LOCATION)
		C.perror(c'Bind failed')
		close_socket(server_fd)
		return -1
	}
	if C.listen(server_fd, max_connection_size) < 0 {
		eprintln(@LOCATION)
		C.perror(c'Listen failed')
		close_socket(server_fd)
		return -1
	}
	return server_fd
}

// add_fd_to_epoll adds a file descriptor to the epoll instance
fn add_fd_to_epoll(epoll_fd int, fd int, events u32) int {
	mut ev := C.epoll_event{
		events: events
	}
	ev.data.fd = fd
	if C.epoll_ctl(epoll_fd, C.EPOLL_CTL_ADD, fd, &ev) == -1 {
		eprintln(@LOCATION)
		C.perror(c'epoll_ctl')
		return -1
	}
	return 0
}

// remove_fd_from_epoll removes a file descriptor from the epoll instance
fn remove_fd_from_epoll(epoll_fd int, fd int) bool {
	ret := C.epoll_ctl(epoll_fd, C.EPOLL_CTL_DEL, fd, C.NULL)
	if ret == -1 {
		eprintln('ERROR: epoll_ctl(DEL, fd=${fd}) failed with errno=${C.errno}')
		return false
	}
	return true
}

fn handle_accept_loop(epoll_fd int, listen_fd int, mut client_fds map[int]bool) {
	for {
		client_fd := C.accept4(listen_fd, C.NULL, C.NULL, C.SOCK_NONBLOCK)
		if client_fd < 0 {
			if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
				break // No more incoming connections; exit loop.
			}
			eprintln(@LOCATION)
			C.perror(c'Accept failed')
			break
		}
		// Enable TCP_NODELAY for lower latency
		opt := 1
		C.setsockopt(client_fd, C.IPPROTO_TCP, C.TCP_NODELAY, &opt, sizeof(opt))
		// Register client socket with epoll
		if add_fd_to_epoll(epoll_fd, client_fd, u32(C.EPOLLIN | C.EPOLLET)) == -1 {
			close_socket(client_fd)
			continue
		}
		client_fds[client_fd] = true
	}
}

fn handle_client_closure(epoll_fd int, client_fd int, mut client_fds map[int]bool, mut client_buffers map[int][]u8) {
	// Never close the listening socket here
	if client_fd == 0 {
		return
	}
	if client_fd <= 0 {
		eprintln('ERROR: Invalid FD=${client_fd} for closure')
		return
	}
	client_fds.delete(client_fd)
	client_buffers.delete(client_fd)
	remove_fd_from_epoll(epoll_fd, client_fd)
	close_socket(client_fd)
}

fn close_worker_clients(epoll_fd int, mut client_fds map[int]bool, mut client_buffers map[int][]u8) {
	for client_fd in client_fds.keys() {
		handle_client_closure(epoll_fd, client_fd, mut client_fds, mut client_buffers)
	}
}

fn process_request(server &Server, epoll_fd int, client_fd int, request_buffer []u8, mut client_fds map[int]bool, mut client_buffers map[int][]u8) {
	server.begin_request()
	defer {
		server.end_request()
	}
	mut decoded_http_request := decode_http_request(request_buffer) or {
		eprintln('Error decoding request ${err}')
		C.send(client_fd, tiny_bad_request_response.data, tiny_bad_request_response.len,
			C.MSG_NOSIGNAL)
		handle_client_closure(epoll_fd, client_fd, mut client_fds, mut client_buffers)
		return
	}
	decoded_http_request.client_conn_fd = client_fd
	decoded_http_request.user_data = server.user_data
	response := server.request_handler(decoded_http_request) or {
		eprintln('Error handling request ${err}')
		C.send(client_fd, tiny_bad_request_response.data, tiny_bad_request_response.len,
			C.MSG_NOSIGNAL)
		handle_client_closure(epoll_fd, client_fd, mut client_fds, mut client_buffers)
		return
	}

	if response.takeover {
		// The handler has taken ownership of the connection.
		// Remove from epoll and tracking, but do NOT close the fd.
		client_fds.delete(client_fd)
		client_buffers.delete(client_fd)
		remove_fd_from_epoll(epoll_fd, client_fd)
		return
	}

	if response.content.len > 0 {
		mut send_error := false
		mut pos := 0
		for pos < response.content.len {
			sent := C.send(client_fd, unsafe { &response.content[pos] },
				response.content.len - pos, C.MSG_NOSIGNAL)
			if sent <= 0 {
				eprintln('ERROR: send() failed with errno=${C.errno}')
				send_error = true
				break
			}
			pos += sent
		}
		if send_error {
			handle_client_closure(epoll_fd, client_fd, mut client_fds, mut client_buffers)
			return
		}
	}

	if response.file_path != '' {
		mut fd := C.open(response.file_path.str, C.O_RDONLY, 0)
		if fd == -1 {
			eprintln('ERROR: open file failed')
			handle_client_closure(epoll_fd, client_fd, mut client_fds, mut client_buffers)
			return
		}
		defer {
			if fd != -1 {
				C.close(fd)
			}
		}
		mut st := C.stat{}
		if C.fstat(fd, &st) != 0 {
			eprintln('ERROR: fstat failed')
			handle_client_closure(epoll_fd, client_fd, mut client_fds, mut client_buffers)
			return
		}
		mut offset := i64(0)
		mut remaining := i64(st.st_size)
		mut sf_retries := 0
		for remaining > 0 {
			ssize := C.sendfile(client_fd, fd, &offset, usize(remaining))
			if ssize > 0 {
				remaining -= i64(ssize)
				sf_retries = 0
				continue
			}
			errno_val := C.errno
			match errno_val {
				C.EAGAIN, C.EWOULDBLOCK, C.EINTR {
					if sf_retries < 3 {
						sf_retries++
						continue
					}
					eprintln('ERROR: sendfile() transient failure after ${sf_retries} retries (errno=${errno_val})')
				}
				C.EBADF {
					eprintln('ERROR: sendfile() EBADF: input fd or socket not open for required access (errno=${errno_val})')
				}
				C.EFAULT {
					eprintln('ERROR: sendfile() EFAULT: bad address for offset (errno=${errno_val})')
				}
				C.EINVAL {
					eprintln('ERROR: sendfile() EINVAL: invalid descriptor state or non-seekable input (errno=${errno_val})')
				}
				C.EIO {
					eprintln('ERROR: sendfile() EIO: I/O error while reading input file (errno=${errno_val})')
				}
				C.ENOMEM {
					eprintln('ERROR: sendfile() ENOMEM: insufficient kernel memory (errno=${errno_val})')
				}
				C.EOVERFLOW {
					eprintln('ERROR: sendfile() EOVERFLOW: count exceeds file/socket limits (errno=${errno_val})')
				}
				C.ESPIPE {
					eprintln('ERROR: sendfile() ESPIPE: input file not seekable with offset (errno=${errno_val})')
				}
				else {
					eprintln('ERROR: sendfile() failed with errno=${errno_val}')
				}
			}

			handle_client_closure(epoll_fd, client_fd, mut client_fds, mut client_buffers)
			return
		}
	}

	client_buffers.delete(client_fd)
	if server.is_shutting_down() || response.should_close {
		handle_client_closure(epoll_fd, client_fd, mut client_fds, mut client_buffers)
	}
}

fn process_events(server &Server, epoll_fd int, listen_fd int) {
	mut events := [max_connection_size]C.epoll_event{}
	mut request_buffer := []u8{len: server.max_request_buffer_size, cap: server.max_request_buffer_size}
	mut client_fds := map[int]bool{}
	mut client_buffers := map[int][]u8{}
	unsafe {
		request_buffer.flags.set(.noslices | .nogrow | .noshrink)
	}
	for {
		if server.is_shutting_down() && server.active_request_count() == 0 {
			close_worker_clients(epoll_fd, mut client_fds, mut client_buffers)
			return
		}
		num_events := C.epoll_wait(epoll_fd, &events[0], max_connection_size, epoll_wait_timeout_ms)
		if num_events < 0 {
			if C.errno == C.EINTR {
				continue
			}
			if server.is_shutting_down() {
				continue
			}
			eprintln('ERROR: epoll_wait() failed with errno=${C.errno}')
			continue
		}
		for i := 0; i < num_events; i++ {
			client_fd := unsafe { events[i].data.fd }
			// Accept new connections when the listening socket is readable
			if client_fd == listen_fd {
				if server.is_shutting_down() {
					continue
				}
				handle_accept_loop(epoll_fd, listen_fd, mut client_fds)
				continue
			}

			if events[i].events & u32((C.EPOLLHUP | C.EPOLLERR)) != 0 {
				if client_fd == listen_fd {
					eprintln('ERROR: listen fd had HUP/ERR')
					continue
				}
				if client_fd > 0 {
					// Try to send 444 No Response before closing abnormal connection
					C.send(client_fd, status_444_response.data, status_444_response.len,
						C.MSG_NOSIGNAL)
					handle_client_closure(epoll_fd, client_fd, mut client_fds, mut client_buffers)
				} else {
					eprintln('ERROR: Invalid FD from epoll: ${client_fd}')
				}
				continue
			}
			if events[i].events & u32(C.EPOLLIN) != 0 {
				if server.is_shutting_down() {
					handle_client_closure(epoll_fd, client_fd, mut client_fds, mut client_buffers)
					continue
				}
				// Read all available data from the socket
				mut total_bytes_read := 0
				mut readed_request_buffer := client_buffers[client_fd] or {
					[]u8{cap: server.max_request_buffer_size}
				}
				mut headers_complete := false
				mut header_too_large := false
				mut header_end_pos := -1
				mut request_complete := false
				mut peer_closed := false
				mut recv_error := false

				for {
					bytes_read := C.recv(client_fd, unsafe { &request_buffer[0] },
						server.max_request_buffer_size - 1, 0)
					if bytes_read < 0 {
						if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
							// No more data available right now
							break
						}
						// Error occurred
						eprintln('ERROR: recv() failed with errno=${C.errno}')
						recv_error = true
						break
					} else if bytes_read == 0 {
						// Connection closed by client
						peer_closed = true
						break
					}

					unsafe {
						readed_request_buffer.push_many(&request_buffer[0], bytes_read)
					}
					total_bytes_read += bytes_read

					// Enforce the configured limit on request headers, not on the whole body.
					buffer_len := readed_request_buffer.len
					if !headers_complete && buffer_len >= 4 {
						header_end_pos = find_header_end_in_buf(readed_request_buffer.data,
							buffer_len)
						if header_end_pos == -1 {
							if buffer_len >= server.max_request_buffer_size {
								header_too_large = true
								break
							}
						} else {
							headers_complete = true
							if header_end_pos > server.max_request_buffer_size {
								header_too_large = true
								break
							}
						}
					}

					if headers_complete && has_complete_body(readed_request_buffer.data, buffer_len) {
						request_complete = true
						break
					}
				}

				if header_too_large {
					C.send(client_fd, status_413_response.data, status_413_response.len,
						C.MSG_NOSIGNAL)
					handle_client_closure(epoll_fd, client_fd, mut client_fds, mut client_buffers)
					continue
				}
				if request_complete {
					process_request(server, epoll_fd, client_fd, readed_request_buffer, mut
						client_fds, mut client_buffers)
				} else if recv_error {
					// Unexpected recv error - send 444 No Response
					C.send(client_fd, status_444_response.data, status_444_response.len,
						C.MSG_NOSIGNAL)
					handle_client_closure(epoll_fd, client_fd, mut client_fds, mut client_buffers)
				} else if peer_closed || (total_bytes_read == 0 && readed_request_buffer.len == 0) {
					// Normal client closure (FIN received)
					handle_client_closure(epoll_fd, client_fd, mut client_fds, mut client_buffers)
				} else if readed_request_buffer.len > 0 {
					client_buffers[client_fd] = readed_request_buffer
				}
			}
		}
	}
}

fn (mut server Server) stop_accepting() {
	for i := 0; i < max_thread_pool_size; i++ {
		if server.listen_fds[i] < 0 {
			continue
		}
		if server.epoll_fds[i] >= 0 {
			remove_fd_from_epoll(server.epoll_fds[i], server.listen_fds[i])
		}
		close_socket(server.listen_fds[i])
		server.listen_fds[i] = -1
	}
}

// run starts the server and begins listening for incoming connections.
pub fn (mut server Server) run() ! {
	$if windows {
		eprintln('Windows is not supported yet')
		return
	}
	for i := 0; i < max_thread_pool_size; i++ {
		server.listen_fds[i] = create_server_socket(server)
		if server.listen_fds[i] < 0 {
			return
		}

		server.epoll_fds[i] = C.epoll_create1(0)
		if server.epoll_fds[i] < 0 {
			C.perror(c'epoll_create1 failed')
			close_socket(server.listen_fds[i])
			return
		}

		// Register the listening socket with each worker epoll for distributed accepts (edge-triggered)
		if add_fd_to_epoll(server.epoll_fds[i], server.listen_fds[i], u32(C.EPOLLIN | C.EPOLLET)) == -1 {
			close_socket(server.listen_fds[i])
			close_socket(server.epoll_fds[i])
			return
		}

		server.threads[i] = spawn process_events(server, server.epoll_fds[i], server.listen_fds[i])
	}

	server.mark_running()
	println('listening on http://0.0.0.0:${server.port}/')
	// Main thread waits for workers; accepts are handled in worker epoll loops
	for i in 0 .. max_thread_pool_size {
		server.threads[i].wait()
		if server.epoll_fds[i] >= 0 {
			close_socket(server.epoll_fds[i])
			server.epoll_fds[i] = -1
		}
	}
	server.mark_stopped()
}
