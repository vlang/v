module main

const port = 3000
const max_thread_pool_size = 8

// handle_request finds and executes the handler for a given route.
// It takes an HttpRequest object as an argument and returns the response as a byte array.
fn handle_request(req HttpRequest) ![]u8 {
	method := unsafe { tos(&req.buffer[req.method.start], req.method.len) }
	path := unsafe { tos(&req.buffer[req.path.start], req.path.len) }

	if method == 'GET' {
		if path == '/' {
			return home_controller([])
		} else if path.starts_with('/user/') {
			id := path[6..]
			return get_user_controller([id])
		}
	} else if method == 'POST' {
		if path == '/user' {
			return create_user_controller([])
		}
	}

	return tiny_bad_request_response
}

fn main() {
	// 0x0202 = MAKEWORD(2,2). it is the version number for Winsock 2.2
	$if windows {
		mut wsa_data := C.WSADATA{}
		if C.WSAStartup(0x0202, &wsa_data) != 0 {
			eprintln('WSAStartup failed')
			return
		}
	}

	mut server := Server{
		request_handler: handle_request
	}

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
		server.threads[i] = spawn process_events(&server)
	}
	println('listening on http://localhost:${port}/')
	event_loop(&server)
}
