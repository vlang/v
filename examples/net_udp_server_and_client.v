import os
import os.cmdline
import net

fn main() {
	println('Usage: net_udp_server_and_client [-l] [-p 5000]')
	println('     -l      - act as a server and listen')
	println('     -p XXXX - custom port number')
	println('------------------------------------------')
	is_server := '-l' in os.args
	port := cmdline.option(os.args, '-p', '40001').int()
	mut buf := []byte{len: 100}
	if is_server {
		println('UDP echo server, listening for udp packets on port: $port')
		mut c := net.listen_udp(port) ?
		for {
			read, addr := c.read(mut buf) or { continue }
			println('received $read bytes from $addr')
			c.write_to(addr, buf[..read]) or {
				println('Server: connection dropped')
				continue
			}
		}
	} else {
		println('UDP client, sending packets to port: ${port}.\nType `exit` to exit.')
		mut c := net.dial_udp('localhost', 'localhost:$port') ?
		for {
			mut line := os.input('client > ')
			match line {
				'' {
					line = '\n'
				}
				'exit' {
					println('goodbye.')
					exit(0)
				}
				else {}
			}
			c.write_string(line) ?
			read, _ := c.read(mut buf) ?
			println('server : ' + buf[0..read].bytestr())
		}
	}
}
