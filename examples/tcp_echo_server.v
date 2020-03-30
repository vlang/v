import net
// This file shows how a basic TCP echo server can be implemented using
// the `net` module. You can connect to the server by using netcat
// or telnet, in separate shells, for example:
// `nc 127.0.0.1 12345`
// `telnet 127.0.0.1 12345`
fn handle_connection(con net.Socket) {
	eprintln('new client connected')
	defer {
		eprintln('closing connection: $con')
		con.close() or { }
	}
	con.send_string("Welcome to V's TCP Echo server.\n") or {
		return
	}
	for {
		line := con.read_line()
		if line.len == 0 {
			return
		}
		eprintln('received line: ' + line.trim_space())
		con.send_string(line) or {
			return
		}
	}
}

fn main() {
	server_port := 12345
	eprintln('Starting an echo server, listening on port: $server_port')
	server := net.listen(server_port) or {
		panic(err)
	}
	for {
		con := server.accept() or {
			server.close() or { }
			panic(err)
		}
		go handle_connection(con)
	}
}
