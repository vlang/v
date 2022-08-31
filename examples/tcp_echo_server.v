import io
import net

// This file shows how a basic TCP echo server can be implemented using
// the net module. You can connect to the server by using netcat or telnet,
// in separate shells, for example:
// nc 127.0.0.1 12345
// or
// telnet 127.0.0.1 12345

fn main() {
	mut server := net.listen_tcp(.ip6, ':12345')?
	laddr := server.addr()?
	eprintln('Listen on $laddr ...')
	for {
		mut socket := server.accept()?
		go handle_client(mut socket)
	}
}

fn handle_client(mut socket net.TcpConn) {
	defer {
		socket.close() or { panic(err) }
	}
	client_addr := socket.peer_addr() or { return }
	eprintln('> new client: $client_addr')
	mut reader := io.new_buffered_reader(reader: socket)
	defer {
		unsafe {
			reader.free()
		}
	}
	socket.write_string('server: hello\n') or { return }
	for {
		received_line := reader.read_line() or { return }
		if received_line == '' {
			return
		}
		println('client $client_addr: $received_line')
		socket.write_string('server: $received_line\n') or { return }
	}
}
