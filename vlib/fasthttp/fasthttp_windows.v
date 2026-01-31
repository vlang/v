module fasthttp

struct Server {
pub:
	port int = 3000
	max_request_buffer_size int = 8192
mut:
	request_handler fn (HttpRequest) !HttpResponse @[required]
}

// new_server creates and initializes a new Server instance.
pub fn new_server(config ServerConfig) !&Server {
	mut server := &Server{
		port:            config.port
		max_request_buffer_size: config.max_request_buffer_size
		request_handler: config.handler
	}

	return server
}

// run starts the server and begins listening for incoming connections.
pub fn (mut server Server) run() ! {
	println('TODO: implement fasthttp.Server.run on windows')
}
