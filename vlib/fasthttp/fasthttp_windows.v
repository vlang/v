module fasthttp

struct Server {
pub:
	port int = 3000
mut:
	request_handler fn (HttpRequest) ![]u8 @[required]
}

// new_server creates and initializes a new Server instance.
pub fn new_server(port int, handler fn (req HttpRequest) ![]u8) !&Server {
	mut server := &Server{
		port:            port
		request_handler: handler
	}

	return server
}

// run starts the server and begins listening for incoming connections.
pub fn (mut server Server) run() ! {
	println('listening on http://localhost:${server.port}/')
}
