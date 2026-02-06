import net
import io

fn main() {
	// Make a new connection
	mut conn := net.dial_tcp('google.com:80')!
	defer {
		conn.close() or {}
	}

	println(' peer: ${conn.peer_addr()!}')
	println('local: ${conn.addr()!}')

	// Simple http HEAD request for a file
	conn.write_string('HEAD /index.html HTTP/1.0\r\n\r\n')!
	// Read all the data that is waiting
	result := io.read_all(reader: conn)!
	// Cast to string and print result
	println(result.bytestr())
}
