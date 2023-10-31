import net
import time

fn echo_server(mut c net.UdpConn) {
	mut count := 0
	for {
		eprintln('> echo_server loop count: ${count}')
		mut buf := []u8{len: 100}
		read, addr := c.read(mut buf) or { continue }
		eprintln('Server got addr ${addr}, read: ${read} | buf: ${buf}')
		c.write_to(addr, buf[..read]) or {
			println('Server: connection dropped')
			return
		}
		count++
		// Normally, after responding, the test will end, but there are sometimes  cases,
		// when the echo_server continued looping, printing messages constantly.
		// The sleep here, is a small precaution against spamming the CI with log messages,
		// when there are network problems, and it allows easier root cause diagnostic, when
		// they do happen:
		time.sleep(1000 * time.millisecond)
	}
}

const server_addr = '127.0.0.1:40003'

fn echo() ! {
	mut c := net.dial_udp(server_addr) or { panic('could not net.dial_udp: ${err}') }
	defer {
		c.close() or {}
	}
	data := 'Hello from vlib/net!'

	c.write_string(data) or { panic('could not write_string: ${err}') }

	mut buf := []u8{len: 100, init: 0}
	read, addr := c.read(mut buf) or { panic('could not read: ${err}') }

	assert read == data.len
	println('Got address ${addr}')
	// Can't test this here because loopback addresses
	// are mapped to other addresses
	// assert addr.str() == '127.0.0.1:30001'

	for i := 0; i < read; i++ {
		assert buf[i] == data[i]
	}

	println('Got "${buf.bytestr()}"')

	c.close()!
}

fn test_udp() {
	mut l := net.listen_udp(server_addr) or { panic('could not listen_udp: ${err}') }

	spawn echo_server(mut l)
	echo() or { panic('could not echo: ${err}') }

	l.close() or {}
}
