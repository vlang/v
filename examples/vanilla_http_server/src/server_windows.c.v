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
import time

#include <winsock2.h>
#include <ws2tcpip.h>
#include <stdio.h>

fn C.WSAStartup(wVersionRequested u16, lpWSAData voidptr) int

fn C.WSACleanup() int

fn C.WSAPoll(fds voidptr, nfds u32, timeout int) int

fn C.accept(sockfd int, addr voidptr, addrlen voidptr) int

fn C.closesocket(s int) int

fn C.ioctlsocket(s int, cmd int, argp &u32) int

fn C.setsockopt(s int, level int, optname int, optval &int, optlen int) int

fn C.bind(sockfd int, addr voidptr, addrlen int) int

fn C.listen(sockfd int, backlog int) int

fn C.recv(sockfd int, buf voidptr, len int, flags int) int

fn C.send(sockfd int, buf voidptr, len int, flags int) int

fn C.perror(msg &char)

struct In_addr {
	s_addr int
}

struct Sockaddr_in {
	sin_family u16
	sin_port   u16
	sin_addr   In_addr
	sin_zero   [8]u8
}

pub struct Server {
mut:
	socket_fd       int
	clients         []int // List of client sockets (for WSAPoll)
	lock_flag       sync.Mutex
	threads         [max_thread_pool_size]thread
	request_handler fn (HttpRequest) ![]u8 @[required]
}

const max_connection_size = 1024
const port = 8080

// typedef struct pollfd {
//   SOCKET fd;
//   SHORT  events;
//   SHORT  revents;
// } WSAPOLLFD, *PWSAPOLLFD, *LPWSAPOLLFD;

struct WSAPollFD {
	fd      int
	events  i16
	revents i16
}

fn set_blocking(fd int, blocking bool) {
	// https://learn.microsoft.com/en-us/windows/win32/api/winsock/nf-winsock-ioctlsocket
	// 0x8004667e is FIONBIO
	mut i_mode := u32(0) // 0 = blocking, 1 = non-blocking
	if !blocking {
		i_mode = 1
	}
	if C.ioctlsocket(fd, C.FIONBIO, &i_mode) != 0 {
		C.perror('ioctlsocket failed'.str)
	}
}

fn close_socket(fd int) {
	C.closesocket(fd)
}

fn create_server_socket(port int) int {
	// Create a socket with non-blocking mode
	server_fd := C.socket(C.AF_INET, C.SOCK_STREAM | C.SOCK_NONBLOCK, 0)
	if server_fd < 0 {
		eprintln(@LOCATION)
		C.perror('Socket creation failed'.str)
		return -1
	}

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

fn handle_accept(server &Server) {
	for {
		client_fd := C.accept(server.socket_fd, C.NULL, C.NULL)
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
			server.clients << client_fd
			server.lock_flag.unlock()
		}
	}
}

fn handle_client_closure(server &Server, client_fd int) {
	unsafe {
		server.lock_flag.lock()
		// Remove client_fd from the clients list.
		for i, fd in server.clients {
			if fd == client_fd {
				server.clients.delete(i)
				break
			}
		}
		server.lock_flag.unlock()
	}
	close_socket(client_fd)
}

fn process_events(server &Server) {
	for {
		// Copy current client sockets into a WSAPollFD array.
		server.lock_flag.lock()
		mut nfds := server.clients.len
		mut pollfds := []WSAPollFD{len: nfds}
		for i, fd in server.clients {
			pollfds[i].fd = fd
			// WSAPoll event mask: on Windows, POLLRDNORM is used for readable sockets.
			pollfds[i].events = 0x0100
			pollfds[i].revents = 0
		}
		server.lock_flag.unlock()

		// If no clients, sleep briefly.
		if nfds == 0 {
			time.sleep(1 * time.second)
			continue
		}

		ret := C.WSAPoll(&pollfds[0], nfds, 1000) // 1‑second timeout
		if ret < 0 {
			eprintln('WSAPoll error')
			continue
		}

		// Process events for each client.
		for i, pfd in pollfds {
			if pfd.revents != 0 {
				mut request_buffer := [140]u8{}
				bytes_read := C.recv(pfd.fd, &request_buffer[0], 140 - 1, 0)
				if bytes_read > 0 {
					mut readed_request_buffer := []u8{cap: bytes_read}
					unsafe {
						readed_request_buffer.push_many(&request_buffer[0], bytes_read)
					}
					// decode_http_request is assumed to be defined elsewhere.
					decoded_http_request := decode_http_request(readed_request_buffer) or {
						eprintln('Error decoding request')
						C.send(pfd.fd, tiny_bad_request_response.data, tiny_bad_request_response.len,
							0)
						handle_client_closure(server, pfd.fd)
						continue
					}

					// This lock is a workaround for avoiding race condition in router.params
					// This slows down the server, but it's a temporary solution
					(*server).lock_flag.lock()
					response_buffer := (*server).request_handler(decoded_http_request) or {
						eprintln('Error handling request')
						C.send(pfd.fd, tiny_bad_request_response.data, tiny_bad_request_response.len,
							0)
						handle_client_closure(server, pfd.fd)
						(*server).lock_flag.unlock()
						continue
					}
					(*server).lock_flag.unlock()
					C.send(pfd.fd, response_buffer.data, response_buffer.len, 0)
					handle_client_closure(server, pfd.fd)
				} else {
					handle_client_closure(server, pfd.fd)
				}
			}
		}
	}
}

fn (mut server Server) run() {
	// 0x0202 = MAKEWORD(2,2). it is the version number for Winsock 2.2

	mut wsa_data := C.WSADATA{}
	if C.WSAStartup(0x0202, &wsa_data) != 0 {
		eprintln('WSAStartup failed')
		return
	}

	server.socket_fd = create_server_socket(port)
	if server.socket_fd < 0 {
		return
	}

	server.lock_flag.init()
	spawn process_events(&server)

	println('listening on http://localhost:${port}/')
	event_loop(&server)
	C.WSACleanup()
}
