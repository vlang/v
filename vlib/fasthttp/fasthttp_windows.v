module fasthttp

struct Server {
pub:
	port int = 3000
mut:
	request_handler fn (HttpRequest) ![]u8 @[required]
}

// new_server creates and initializes a new Server instance.
pub fn new_server(config ServerConfig) !&Server {
	mut server := &Server{
		port:            config.port
		request_handler: config.handler
	}

	return server
}

// run starts the server and begins listening for incoming connections.
pub fn (mut server Server) run() ! {
	println('TODO: implement fasthttp.Server.run on windows')
}
