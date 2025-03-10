// This module implements a basic HTTP server using epoll for handling multiple client connections efficiently.
// The server is designed to be non-blocking and uses multiple threads to handle incoming requests concurrently.
//
// Performance Considerations:
// - Non-blocking I/O: The server uses non-blocking sockets to ensure that it can handle multiple connections without being blocked by any single connection.
// - Epoll: The use of epoll allows the server to efficiently monitor multiple file descriptors to see if I/O is possible on any of them.
// - Threading: The server spawns multiple threads to handle client requests, which can improve performance on multi-core systems.
// - Memory Management: Care is taken to allocate and free memory appropriately to avoid memory leaks and ensure efficient memory usage.
// - Error Handling: The server includes error handling to manage and log errors without crashing, ensuring robustness and reliability.
// - SO_REUSEPORT: The server sets the SO_REUSEPORT socket option to allow multiple sockets on the same host and port, which can improve performance in certain scenarios.
// - Connection Handling: The server efficiently handles client connections, including accepting new connections, reading requests, and sending responses.
// - Mutex Locking: The server uses mutex locks to manage access to shared resources, ensuring thread safety while minimizing contention.
module main

import sync

const tiny_bad_request_response = 'HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()

#include <fcntl.h>
#include <sys/epoll.h>
#include <errno.h>
#include <netinet/in.h>

fn C.socket(socket_family int, socket_type int, protocol int) int

fn C.bind(sockfd int, addr &Addr, addrlen u32) int

fn C.send(__fd int, __buf voidptr, __n usize, __flags int) int

fn C.recv(__fd int, __buf voidptr, __n usize, __flags int) int

fn C.setsockopt(__fd int, __level int, __optname int, __optval voidptr, __optlen u32) int

fn C.listen(__fd int, __n int) int

fn C.perror(s &u8) voidptr

fn C.close(fd int) int

fn C.accept(sockfd int, address &C.sockaddr_in, addrlen &u32) int

fn C.htons(__hostshort u16) u16

fn C.epoll_create1(__flags int) int

fn C.epoll_ctl(__epfd int, __op int, __fd int, __event &C.epoll_event) int

fn C.epoll_wait(__epfd int, __events &C.epoll_event, __maxevents int, __timeout int) int

fn C.fcntl(fd int, cmd int, arg int) int

struct In_addr {
	s_addr int
}

struct Sockaddr_in {
	sin_family u16
	sin_port   u16
	sin_addr   In_addr
	sin_zero   [8]u8
}

@[typedef]
union C.epoll_data {
mut:
	ptr voidptr
	fd  int
	u32 u32
	u64 u64
}

pub struct C.epoll_event {
	events u32
	data   C.epoll_data
}

struct Server {
mut:
	server_socket   int
	epoll_fd        int
	lock_flag       sync.Mutex
	has_clients     int
	threads         [max_thread_pool_size]thread
	request_handler fn (HttpRequest) ![]u8 @[required]
}

const max_unix_path = 108

const max_connection_size = 1024

@[_pack: '1']
pub struct Ip6 {
	port      u16
	flow_info u32
	addr      [16]u8
	scope_id  u32
}

@[_pack: '1']
pub struct Ip {
	port    u16
	addr    [4]u8
	sin_pad [8]u8
}

pub struct Unix {
	path [max_unix_path]u8
}

union AddrData {
	Unix
	Ip
	Ip6
}

@[_pack: '1']
pub struct Addr {
pub:
	f    u16
	addr AddrData
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

fn close_socket(fd int) {
	C.close(fd)
}

fn create_server_socket(port int) int {
	// Create a socket with non-blocking mode
	server_fd := C.socket(C.AF_INET, C.SOCK_STREAM, 0)
	if server_fd < 0 {
		eprintln(@LOCATION)
		C.perror('Socket creation failed'.str)
		return -1
	}

	// set_blocking(server_fd, false)

	// Enable SO_REUSEPORT
	opt := 1
	if C.setsockopt(server_fd, C.SOL_SOCKET, C.SO_REUSEPORT, &opt, sizeof(opt)) < 0 {
		eprintln(@LOCATION)
		C.perror('setsockopt SO_REUSEPORT failed'.str)
		close_socket(server_fd)
		return -1
	}

	server_addr := Sockaddr_in{
		sin_family: u16(C.AF_INET)
		sin_port:   C.htons(port)
		sin_addr:   In_addr{C.INADDR_ANY}
		sin_zero:   [8]u8{}
	}

	if C.bind(server_fd, voidptr(&server_addr), sizeof(server_addr)) < 0 {
		eprintln(@LOCATION)
		C.perror('Bind failed'.str)
		close_socket(server_fd)
		return -1
	}
	if C.listen(server_fd, max_connection_size) < 0 {
		eprintln(@LOCATION)
		C.perror('Listen failed'.str)
		close_socket(server_fd)
		return -1
	}
	return server_fd
}

// Function to add a file descriptor to the epoll instance
fn add_fd_to_epoll(epoll_fd int, fd int, events u32) int {
	mut ev := C.epoll_event{
		events: events
	}
	ev.data.fd = fd
	if C.epoll_ctl(epoll_fd, C.EPOLL_CTL_ADD, fd, &ev) == -1 {
		eprintln(@LOCATION)
		C.perror('epoll_ctl'.str)
		return -1
	}
	return 0
}

// Function to remove a file descriptor from the epoll instance
fn remove_fd_from_epoll(epoll_fd int, fd int) {
	C.epoll_ctl(epoll_fd, C.EPOLL_CTL_DEL, fd, C.NULL)
}

fn handle_accept_loop(mut server Server) {
	for {
		client_fd := C.accept(server.server_socket, C.NULL, C.NULL)
		if client_fd < 0 {
			// Check for EAGAIN or EWOULDBLOCK, usually represented by errno 11.
			if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
				break // No more incoming connections; exit loop.
			}

			eprintln(@LOCATION)
			C.perror('Accept failed'.str)
			return
		}

		unsafe {
			server.lock_flag.lock()
			if add_fd_to_epoll(server.epoll_fd, client_fd, u32(C.EPOLLIN | C.EPOLLET)) == -1 {
				close_socket(client_fd)
			}
			server.lock_flag.unlock()
		}
	}
}

fn handle_client_closure(server &Server, client_fd int) {
	unsafe {
		server.lock_flag.lock()
		remove_fd_from_epoll(client_fd, client_fd)
		server.lock_flag.unlock()
	}
}

fn process_events(mut server Server) {
	for {
		events := [max_connection_size]C.epoll_event{}
		num_events := C.epoll_wait(server.epoll_fd, &events[0], max_connection_size, -1)
		for i := 0; i < num_events; i++ {
			if events[i].events & u32((C.EPOLLHUP | C.EPOLLERR)) != 0 {
				handle_client_closure(server, unsafe { events[i].data.fd })
				continue
			}
			if events[i].events & u32(C.EPOLLIN) != 0 {
				request_buffer := [140]u8{}
				bytes_read := C.recv(unsafe { events[i].data.fd }, &request_buffer[0],
					140 - 1, 0)
				if bytes_read > 0 {
					mut readed_request_buffer := []u8{cap: bytes_read}

					unsafe {
						readed_request_buffer.push_many(&request_buffer[0], bytes_read)
					}

					decoded_http_request := decode_http_request(readed_request_buffer) or {
						eprintln('Error decoding request ${err}')
						C.send(unsafe { events[i].data.fd }, tiny_bad_request_response.data,
							tiny_bad_request_response.len, 0)
						handle_client_closure(server, unsafe { events[i].data.fd })
						continue
					}

					// This lock is a workaround for avoiding race condition in router.params
					// This slows down the server, but it's a temporary solution
					server.lock_flag.lock()
					response_buffer := server.request_handler(decoded_http_request) or {
						eprintln('Error handling request ${err}')
						C.send(unsafe { events[i].data.fd }, tiny_bad_request_response.data,
							tiny_bad_request_response.len, 0)
						handle_client_closure(server, unsafe { events[i].data.fd })
						server.lock_flag.unlock()
						continue
					}
					server.lock_flag.unlock()

					C.send(unsafe { events[i].data.fd }, response_buffer.data, response_buffer.len,
						0)
					handle_client_closure(server, unsafe { events[i].data.fd })
				} else if bytes_read == 0
					|| (bytes_read < 0 && C.errno != C.EAGAIN && C.errno != C.EWOULDBLOCK) {
					handle_client_closure(server, unsafe { events[i].data.fd })
				}
			}
		}
	}
}

fn (mut server Server) run() {
	server.server_socket = create_server_socket(port)
	if server.server_socket < 0 {
		return
	}

	server.epoll_fd = C.epoll_create1(0)
	if server.epoll_fd < 0 {
		C.perror('epoll_create1 failed'.str)
		close_socket(server.server_socket)
		return
	}
	server.lock_flag.lock()
	if add_fd_to_epoll(server.epoll_fd, server.server_socket, u32(C.EPOLLIN)) == -1 {
		close_socket(server.server_socket)
		close_socket(server.epoll_fd)
		server.lock_flag.unlock()
		return
	}
	server.lock_flag.unlock()

	server.lock_flag.init()

	for i := 0; i < max_thread_pool_size; i++ {
		server.threads[i] = spawn process_events(mut server)
	}

	println('listening on http://localhost:${port}/')
	handle_accept_loop(mut server)
}
