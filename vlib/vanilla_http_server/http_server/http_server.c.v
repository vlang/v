module http_server

import runtime

const max_connection_size = 1024
const max_thread_pool_size = runtime.nr_cpus()
pub const tiny_bad_request_response = 'HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
const status_444_response = 'HTTP/1.1 444 No Response\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
const status_499_response = 'HTTP/1.1 499 Client Closed Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()

#include <fcntl.h>
#include <errno.h>

$if !windows {
	#include <sys/epoll.h>
	#include <netinet/in.h>
}

fn C.socket(socket_family int, socket_type int, protocol int) int
fn C.bind(sockfd int, addr &C.sockaddr_in, addrlen u32) int
fn C.send(__fd int, __buf voidptr, __n usize, __flags int) int
fn C.recv(__fd int, __buf voidptr, __n usize, __flags int) int
fn C.setsockopt(__fd int, __level int, __optname int, __optval voidptr, __optlen u32) int
fn C.listen(__fd int, __n int) int
fn C.perror(s &u8)
fn C.close(fd int) int
fn C.accept(sockfd int, address &C.sockaddr_in, addrlen &u32) int
fn C.htons(__hostshort u16) u16
fn C.epoll_create1(__flags int) int
fn C.epoll_ctl(__epfd int, __op int, __fd int, __event &C.epoll_event) int
fn C.epoll_wait(__epfd int, __events &C.epoll_event, __maxevents int, __timeout int) int
fn C.fcntl(fd int, cmd int, arg int) int

struct C.in_addr {
	s_addr u32
}

struct C.sockaddr_in {
	sin_family u16
	sin_port   u16
	sin_addr   C.in_addr
	sin_zero   [8]u8
}

union C.epoll_data {
	ptr voidptr
	fd  int
	u32 u32
	u64 u64
}

struct C.epoll_event {
	events u32
	data   C.epoll_data
}

pub struct Server {
pub:
	port int = 3000
pub mut:
	socket_fd       int
	epoll_fds       []int    = []int{len: max_thread_pool_size, cap: max_thread_pool_size}
	threads         []thread = []thread{len: max_thread_pool_size, cap: max_thread_pool_size}
	request_handler fn ([]u8, int) ![]u8 @[required]
}

fn set_blocking(fd int, blocking bool) {
	flags := C.fcntl(fd, C.F_GETFL, 0)
	if flags == -1 {
		eprintln(@LOCATION)
		return
	}
	new_flags := if blocking { flags & ~C.O_NONBLOCK } else { flags | C.O_NONBLOCK }
	C.fcntl(fd, C.F_SETFL, new_flags)
}

fn close_socket(fd int) {
	C.close(fd)
}

fn create_server_socket(port int) int {
	server_fd := C.socket(C.AF_INET, C.SOCK_STREAM, 0)
	if server_fd < 0 {
		eprintln(@LOCATION)
		C.perror(c'Socket creation failed')
		exit(1)
	}

	set_blocking(server_fd, false)

	opt := 1
	if C.setsockopt(server_fd, C.SOL_SOCKET, C.SO_REUSEPORT, &opt, sizeof(opt)) < 0 {
		eprintln(@LOCATION)
		C.perror(c'setsockopt SO_REUSEPORT failed')
		close_socket(server_fd)
		exit(1)
	}

	server_addr := C.sockaddr_in{
		sin_family: u16(C.AF_INET)
		sin_port:   C.htons(port)
		sin_addr:   C.in_addr{u32(C.INADDR_ANY)}
		sin_zero:   [8]u8{}
	}

	if C.bind(server_fd, &server_addr, sizeof(server_addr)) < 0 {
		eprintln(@LOCATION)
		C.perror(c'Bind failed')
		close_socket(server_fd)
		exit(1)
	}

	if C.listen(server_fd, max_connection_size) < 0 {
		eprintln(@LOCATION)
		C.perror(c'Listen failed')
		close_socket(server_fd)
		exit(1)
	}

	return server_fd
}

fn create_epoll_fd() int {
	epoll_fd := C.epoll_create1(0)
	if epoll_fd < 0 {
		C.perror(c'epoll_create1')
	}
	return epoll_fd
}

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

fn remove_fd_from_epoll(epoll_fd int, fd int) {
	C.epoll_ctl(epoll_fd, C.EPOLL_CTL_DEL, fd, C.NULL)
}

fn handle_accept_loop(mut server Server, main_epoll_fd int) {
	mut next_worker := 0
	mut event := C.epoll_event{}

	for {
		num_events := C.epoll_wait(main_epoll_fd, &event, 1, -1)
		if num_events < 0 {
			if C.errno == C.EINTR {
				continue
			}
			C.perror(c'epoll_wait')
			break
		}

		if num_events > 1 {
			eprintln('More than one event in epoll_wait, this should not happen.')
			continue
		}

		if event.events & u32(C.EPOLLIN) != 0 {
			for {
				client_conn_fd := C.accept(server.socket_fd, C.NULL, C.NULL)
				if client_conn_fd < 0 {
					// Check for EAGAIN or EWOULDBLOCK, usually represented by errno 11.
					if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
						break // No more incoming connections; exit loop.
					}
					eprintln(@LOCATION)
					C.perror(c'Accept failed')
					continue
				}
				set_blocking(client_conn_fd, false)
				// Load balance the client connection to the worker threads.
				// this is a simple round-robin approach.
				epoll_fd := server.epoll_fds[next_worker]
				next_worker = (next_worker + 1) % max_thread_pool_size
				if add_fd_to_epoll(epoll_fd, client_conn_fd, u32(C.EPOLLIN | C.EPOLLET)) < 0 {
					close_socket(client_conn_fd)
					continue
				}
			}
		}
	}
}

@[direct_array_access; manualfree]
fn process_events(mut server Server, epoll_fd int) {
	mut events := [max_connection_size]C.epoll_event{}

	for {
		num_events := C.epoll_wait(epoll_fd, &events[0], max_connection_size, -1)
		if num_events < 0 {
			if C.errno == C.EINTR {
				continue
			}
			eprintln(@LOCATION)
			C.perror(c'epoll_wait')
			break
		}

		for i in 0 .. num_events {
			client_conn_fd := unsafe { events[i].data.fd }
			if events[i].events & u32(C.EPOLLHUP | C.EPOLLERR) != 0 {
				remove_fd_from_epoll(epoll_fd, client_conn_fd)
				close_socket(client_conn_fd)
				continue
			}

			if events[i].events & u32(C.EPOLLIN) != 0 {
				mut request_buffer := []u8{}
				defer {
					unsafe {
						request_buffer.free()
					}
				}
				mut temp_buffer := [140]u8{}
				for {
					bytes_read := C.recv(client_conn_fd, &temp_buffer[0], temp_buffer.len,
						0)
					if bytes_read < 0 {
						if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
							break // No more data to read
						}
						eprintln(@LOCATION)
						C.perror(c'recv')
						remove_fd_from_epoll(epoll_fd, client_conn_fd)
						close_socket(client_conn_fd)
						break
					}
					if bytes_read == 0 {
						// Client closed the connection
						remove_fd_from_epoll(epoll_fd, client_conn_fd)
						close_socket(client_conn_fd)
						break
					}
					unsafe { request_buffer.push_many(&temp_buffer[0], bytes_read) }
					if bytes_read < temp_buffer.len {
						break // Assume the request is complete
					}
				}

				if request_buffer.len == 0 {
					C.send(client_conn_fd, status_444_response.data, status_444_response.len,
						0)
					continue
				}

				response_buffer := server.request_handler(request_buffer, client_conn_fd) or {
					eprintln('Error handling request: ${err}')
					C.send(client_conn_fd, tiny_bad_request_response.data, tiny_bad_request_response.len,
						0)
					remove_fd_from_epoll(epoll_fd, client_conn_fd)
					close_socket(client_conn_fd)
					continue
				}

				sent := C.send(client_conn_fd, response_buffer.data, response_buffer.len,
					C.MSG_NOSIGNAL | C.MSG_ZEROCOPY)
				if sent < 0 && C.errno != C.EAGAIN && C.errno != C.EWOULDBLOCK {
					eprintln(@LOCATION)
					C.perror(c'send')
					remove_fd_from_epoll(epoll_fd, client_conn_fd)
					close_socket(client_conn_fd)
				}
			}
		}
	}
}

// run starts the HTTP server and handles incoming connections.
// This method uses epoll for efficient event-driven I/O handling.
pub fn (mut server Server) run() {
	$if windows {
		eprintln('Windows is not supported yet. Please, use WSL or Linux.')
		exit(1)
	}

	server.socket_fd = create_server_socket(server.port)
	if server.socket_fd < 0 {
		return
	}

	main_epoll_fd := create_epoll_fd()
	if main_epoll_fd < 0 {
		close_socket(server.socket_fd)
		exit(1)
	}

	if add_fd_to_epoll(main_epoll_fd, server.socket_fd, u32(C.EPOLLIN)) < 0 {
		close_socket(server.socket_fd)
		close_socket(main_epoll_fd)
		exit(1)
	}

	for i in 0 .. max_thread_pool_size {
		server.epoll_fds[i] = create_epoll_fd()
		if server.epoll_fds[i] < 0 {
			C.perror(c'epoll_create1')
			for j in 0 .. i {
				close_socket(server.epoll_fds[j])
			}
			close_socket(main_epoll_fd)
			close_socket(server.socket_fd)
			exit(1)
		}

		server.threads[i] = spawn process_events(mut server, server.epoll_fds[i])
	}

	println('listening on http://localhost:${server.port}/')
	handle_accept_loop(mut server, main_epoll_fd)
}
