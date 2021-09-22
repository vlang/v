import net

fn echo_server(mut c net.UdpConn) {
	for {
		mut buf := []byte{len: 100, init: 0}
		read, addr := c.read(mut buf) or { continue }

		println('Server got addr $addr')

		c.write_to(addr, buf[..read]) or {
			println('Server: connection dropped')
			return
		}
	}
}

const (
	local_addr  = ':40003'
	remote_addr = 'localhost:40003'
)

fn echo() ? {
	mut c := net.dial_udp(remote_addr) ?
	defer {
		c.close() or {}
	}
	data := 'Hello from vlib/net!'

	c.write_string(data) ?

	mut buf := []byte{len: 100, init: 0}
	read, addr := c.read(mut buf) ?

	assert read == data.len
	println('Got address $addr')
	// Can't test this here because loopback addresses
	// are mapped to other addresses
	// assert addr.str() == '127.0.0.1:30001'

	for i := 0; i < read; i++ {
		assert buf[i] == data[i]
	}

	println('Got "$buf.bytestr()"')

	c.close() ?
}

fn test_udp() {
	mut l := net.listen_udp(local_addr) or {
		println(err)
		assert false
		panic('')
	}

	go echo_server(mut l)
	echo() or {
		println(err)
		assert false
	}

	l.close() or {}
}

fn main() {
	test_udp()
}
