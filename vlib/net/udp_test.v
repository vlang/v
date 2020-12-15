import net

fn echo_server(_c net.UdpConn) {
	mut c := _c
	for {
		mut buf := []byte{ len: 100, init: 0 }
		read, addr := c.read(mut buf) or {
			continue
		}

		c.write_to(addr, buf[..read]) or {
			println('Server: connection dropped')
			return
		}
	}
}

fn echo() ? {
	mut c := net.dial_udp('127.0.0.1:40003', '127.0.0.1:40001')?
	defer { c.close() or { } }
	data := 'Hello from vlib/net!'

	c.write_str(data)?

	mut buf := []byte{ len: 100, init: 0 }
	read, addr := c.read(mut buf)?

	assert read == data.len
	println('Got address $addr')
	// Can't test this here because loopback addresses
	// are mapped to other addresses
	// assert addr.str() == '127.0.0.1:30001'

	for i := 0; i < read; i++ {
		assert buf[i] == data[i]
	}

	println('Got "${buf.bytestr()}"')

	c.close()?

	return none
}

fn test_udp() {
	l := net.listen_udp(40001) or {
		println(err)
		assert false
		panic('')
	}

	go echo_server(l)
	echo() or {
		println(err)
		assert false
	}

	l.close() or { }
}

fn main() {
	test_udp()
}
