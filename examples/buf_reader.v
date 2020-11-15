// Simple raw HTTP head request
import net
import time
import io

// Make a new connection
mut conn := net.dial_tcp('google.com:80')?
// Simple http HEAD request for a file
conn.write_str('GET /index.html HTTP/1.0\r\n\r\n')?
// Make sure to set a timeout so we can wait for a response!
conn.set_read_timeout(10 * time.second)
// Wrap in a buffered reader
mut r := io.new_buffered_reader(reader: io.make_reader(conn))
for {
	l := r.read_line() or {
		break
	}
	println('$l')
	// Make it nice and obvious that we are doing this line by line
	time.sleep_ms(10)
}
