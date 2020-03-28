import net

fn handle_connection(con net.Socket) {
	if _ := con.send_string("Welcome to V's TCP Echo server.\n") {
		for	 {
			line := con.read_line()
			if line.len == 0 { break }
			eprintln('received line: ' + line.trim_space())
			con.send_string(line) or { break }
		}
	}
	con.close() or {}
	return
}

fn main() {
	server_port := 12345
	eprintln('Starting an echo server, listening on port: $server_port')
	server := net.listen(server_port) or {
		panic(err)
	}
	for	 {
		con := server.accept() or {
			server.close() or {}
			panic(err)
		}
		go handle_connection(con)
	}
}
